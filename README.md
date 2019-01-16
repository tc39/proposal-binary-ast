# Binary AST Proposal Overview

Shu-yu Guo (Bloomberg)

David Teller, Kannan Vijayan (Mozilla)

Vladan Djeric (Facebook)

Ingvar Stepanyan (Cloudflare)

This is the explainer document for a proposed new binary AST format for JS.

## Motivation

Performance of applications on the web platform is becoming increasingly bottlenecked by startup (load) time. Larger amounts of JS code are transferred over the wire by more sophisticated web properties. While caching helps, these properties regularly release new code, and cold load times are very important.

A brief survey from July 2017 of the uncompressed JS payload sizes for some popular web applications on desktop and mobile:

| Web App (Desktop) | Uncompressed JS Size |
| ----------------- | -------------------- |
| LinkedIn          | 7.2 MB               |
| Facebook          | 7.1 MB               |
| Google Sheets     | 5.8 MB               |
| Gmail             | 3.9 MB               |
| Yahoo             | 3.4 MB               |

| Web App (Mobile) | Uncompressed JS Size |
| ---------------- | -------------------- |
| LinkedIn         | 6.2 MB               |
| YouTube          | 1.9 MB               |
| Twitter          | 1.8 MB               |
| Facebook         | 1.8 MB               |
| Reddit           | 1.3 MB               |

Startup performance degrades with larger JS payloads, even if only a fraction of the code is actually executed. Parsing time is a significant component, taking more CPU time than bytecode / initial JIT code generation. For example, on a powerful laptop, Chrome spends 10% to 15% of CPU time parsing JS while loading facebook.com.

We propose a new over-the-wire format for JS that is a binary encoding of an AST. We believe this new format would allow for drastically faster parsing. Moreover, web developers are well positioned to adopt a new format as they have embraced build tooling.

We have also implemented a prototype encoder and decoder that demonstrates encouraging performance improvements without bloating file size.

## Design Philosophy

The overriding design philosophy is to be conservative, so as to be realistic both in implementation and committee. This proposal is intended to be simply an alternative encoding of the *surface syntax* with the smallest possible delta to enable high performance parsing. By design, this proposal does not attempt any semantics-level encoding (e.g., bytecode, encoding variables instead of identifiers).

That said, this proposal is highly ambitious.

## Current Parsing Bottlenecks

### <a name="problem1"></a> 1. Information Not Available Where Needed 

One fundamental issue making parsing slow is that information needed to make decisions during parsing is oftentimes not yet available at the point in the input stream at which it is needed. This happens when the parser needs information from code it either hasn't parsed yet (like variable hoisting) or code it doesn't want to parse (like inner functions).

One concrete example is the problem of efficiently representing bindings. In order to make decisions about how to represent or allocate a variable, the parser needs to know whether the variable is closed over, so it becomes necessary to parse any nested functions. The specification only states where a declaration like `var x` needs to be reachable, not how to allocate it -- the information needed for the allocation decision is not encoded where the variable is declared, nor at the spot where it needs to be allocated.

In the following example, due to variable hoisting, `use_x` closes over `x`, which is not known to be declared in `f` at the point of parsing the inner function. An engine cannot emit optimized accesses to `x` inside `use_x` until the entirety of `f` is parsed.

```javascript
var x;
function f() {
  function use_x() {
    use(x); // Closes over hoisted var x.
  }

  var x;
}
```

In the following example, an engine's frontend would like to know how to efficiently allocate space for the `var` declarations as they are parsed. That is not possible without parsing the rest of the function.

```javascript
function g() {
  var x; // Not closed over, should go on function frame.
  var y; // Closed over, should go on activation object.

  return (function() { use(y); });
}
```

In the following example, the presence of `eval` at the bottom of the function is unknown at the time when the engine needs to decide if `var x` may be accessed dynamically.

```javascript
function h(input) {
  var x;

  (function() {
    eval(input);
  })();
}
```

### <a name="problem2"></a> 2. Early Error Semantics

JavaScript’s early error semantics requires the entirety of every file be parsed. Engines employ a lazy parsing (also known as pre-parsing) optimization that avoids building a full AST by skipping initial code generation for inner functions until time of first invocation. This is only 50% faster on average, and parsing effort is still proportional to file size. What's worse, this optimization can backfire. If an inner function whose code generation was skipped happens to be called during application startup, then the engine must in effect re-parse the entire function. This happens often enough that developers have resorted to using immediately-invoked function expressions to altogether bypass lazy parsing.

Consider the following. Currently, since `innerWithSyntaxError` has an early error, `outer` also must throw the early error.

```javascript
function outer() {
  function innerWithSyntaxError() {
    var;
  }
}
```

The proposed behavior change is analogous to wrapping function bodies with `eval`, as below.

```javascript
function outer() {
  function innerWithSyntaxError() {
    eval("var");
  }
}
```

### <a name="problem3"></a> 3. Inefficiencies in Using Characters

When parsing, there can be ambiguity at the character-level about what type of expression a JavaScript syntax is encoding. For example, to distinguish between list expressions and the parameter list for an arrow function, parsers need to do additional bookkeeping to account for all valid interpretations until the ambiguity is resolved, or they need to have the ability to backtrack while parsing.

Similarly, lexing is slow due to reasons such as Unicode encoding and having to handle Unicode escape codes. Engines need to check if some text is single-byte or double-byte encoded, for instance.

## Proposal

We propose a binary encoding based on an efficient abstract syntax tree representation of JavaScript syntax. This is a new, alternate encoding of the surface syntax, with a fairly close bidirectional mapping to the text representation. We seek to be as conservative as possible in introducing new semantics, which are currently limited to:

- deferring early errors
- changing `Function.prototype.toString` behavior
- requiring UTF-8

To further speed up parsing, we propose to encode static semantics as implementation hints, and verify them as deferred assertions.

For this AST format, we currently do not propose to automatically derive from the ECMA-262 grammar since it is too verbose to be an efficient tree representation, and as such it would require a lot of flattening and inlining. Additionally, this might restrict the evolution of the JavaScript specification by effectively making the grammar a normative specification of the binary format.

Instead, we propose a separate tree grammar, with annotations on AST nodes that only express information about the syntax. There are several existing tree grammars which we may take as a starting point, such as Babylon or Shift AST.

(Two grammars is likely to be a contentious issue; this decision is not set in stone and feedback is welcome.)

Borrowing from the WebAssembly approach, the binary encoding would be split into 3 layers:

1. A simple binary encoding of the AST nodes using basic primitives (e.g., strings, numbers, tuples)
2. Additional structural compression on top of the previous layer, leveraging knowledge about the nature of the format of the file and the AST (e.g., constant tables)
3. A generic compression algorithm like gzip or Brotli.

We expect the format to be output by existing compilers such as Babel and TypeScript, and by bundlers such as WebPack.

### Grammar

The tree grammar consists of the primitives (booleans, strings, numbers), lists, and disjoint unions. Disjoint unions are tagged and have a fixed, expected structure (ordered properties) for a given file. Certain nodes, such as strings and lists, are encoded together with their encoded byte length, allowing parsers to skip past the encoded representation of the node.

### Language Evolution and Versioning

The grammar provides the set of all possible node kinds and their ordered properties, which we expect to be monotonically growing. However, not all vendors will support all nodes at any given time. To this end, each file contains a header containing a list of nodes and their properties that the file expects to be used. For example, suppose the grammar currently provides a `FunctionExpression` node that includes an `isAsync` property. If a file expects async functions to be supported, it would include a `FunctionExpression` entry that includes `isAsync` in the list of properties. If a node or property in the header is not supported by the implementation, an error is thrown.

The header solves the versioning problem, forwards compatibility, and backwards compatibility. It also maintains the expected behavior of JavaScript parsers with respect to new features, insofar as parsing a correctly formatted file fails if a parser encounters a feature that the engine does not implement, rather than relying upon a monolithic version number. Finally, it acts as structural compression: node kinds are to be referred to by their index in the header instead of their name.

To save file space, presets will be supported (e.g., ES2015).

### Static Semantics as Annotations

To address concern 1 [Information Not Available Where Needed](#problem1) above, the insight is that all the currently known cases of "information not available where needed" are binding-related. Scope nodes, like function or lexical scope nodes, are able to encode additional annotations about the properties of the AST.

A non-exhaustive list of annotations currently considered:

1. VarDeclaredNames
2. LexicallyDeclaredNames
3. ClosedOverNames (new)
4. AssignedToOutsideOfDeclarationNames (new)
5. Has CallExpression where the callee expression is IdentifierReference of string literal `eval` (new)

These annotations, if present, behave as lazy assertions. By the time the scope node is finished being traversed (which may be arbitrarily delayed in case a skipped inner function is never invoked), if the encoded annotation is contrary to the body of the node, a runtime error is thrown. For example, if a function body is annotated with "has VarDeclaredNames `x`", but does not in fact contain a var declaration with name `x`, an error would be thrown.

In order to prevent divergence across implementations with regards to timing, such errors are required to be thrown when the nearest enclosing function is invoked or the nearest enclosing global script is evaluated.

It is important to note that these annotations must be checked and are not, strictly speaking, hints that may be ignored. They are assertions on the structure of the syntax tree, and must be verified whether or not an engine chooses to use them as parsing hints.

These annotations would be specified as existing or new static semantics.

Ultimately, this list is ad-hoc and the union of properties that various engines currently derive during parsing. With these annotations, engines may generate code for functions in a single forward pass without parsing the functions in entirety. If the needed hints are not present for an engine to do the single-pass fast path, the fallback is the existing analysis that engines already implement. Should the need for new annotations arise, we expect them to be standardized as new static semantics.

Annotations and scopes are embedded in the AST just as any other node properties and use the same versioning mechanism.

### Deferred Early Errors

To address concern 2 [Early Error Semantics](#problem2), Early Errors, like annotation verification, will be deferred to until when the nearest enclosing function is invoked or the nearest enclosing global script is evaluated.

This is a breaking change from textual JS.

Since this format is expected to be compiler output, early errors would be caught at compile time in practice.

### UTF-8 with no escape codes

To address concern 3 [Inefficiencies in Using Characters](#problem3), strings and identifiers will be required to be in UTF-8 without support for escape codes. Again, we expect this to be feasible as the format will be compiler output.

### Verification

Verification of the header, annotations, and the tree structure itself (e.g., its length-encoded nodes, the allowed kinds for child nodes) is intended to be possible in a single forward pass as constant work per node, as the AST is being decoded.

### Function.prototype.toString()

This method would return something like `"[sourceless code]"`.

### Exception Offsets

Thrown exceptions, instead of having a line and column number, will have a byte offset into the binary AST file.

### Further Improvements

It is straightforward to layer additional improvements on this format in order to further improve parsing speed or binary file size. For example, the format could allow for a string table, or allow function bodies to be stored separately from function declarations, and so on.

## Prototype

We implemented an early prototype in Mozilla’s SpiderMonkey engine, by using a grammar based on internal AST format. This was done for speed of implementation, and our next prototype will be [based on the Babylon AST](https://git.io/fN57g).

For the [facebook.com static newsfeed benchmark](https://git.io/vQ1aK), the binary AST representation was slightly smaller than the original JavaScript. This held true even after both representations were passed through gzip for compression. The size reduction mainly came from the use of a string table and variable-length identifiers for entries in the table. It also used variable-length encodings for representing numbers. Additional size wins are possible by leveraging domain-specific information, such as factoring out common subtrees.

The time required to create a full AST (without verifying annotations) was reduced by ~70-90%, which is a considerable reduction since SpiderMonkey's AST construction time for the plain JavaScript was 500-800 ms for the benchmark.

## FAQ

**Why not ship bytecode instead?**

On the Web, no vendor would agree to ship bytecode.

1. Engines don’t want to be tied to a bytecode version.
2. It’s harder to verify bytecode, as bytecode is more expressive than syntax.
3. It runs the risk of bifurcating the language, as bytecode may be more expressive than structured JavaScript source.
4. Designing a new bytecode is an even more ambitious undertaking.

**Why not use WebAssembly?**

There are massive existing untyped JS codebases, and there is no easy way to convert an untyped, garbage collected language like JS to WebAssembly. Given that currently using WebAssembly to ship JavaScript most likely involves bundling a JS runtime as well, it is unclear such a setup would net any load-time performance improvements.

**Why not a semantic graph? Or why not go further? Why not types?**

We do not want to invent a new language by adding new front-end semantics, nor do we want to encode analysis result and require more sophisticated verification that this information is correct.

**Won't this be a massive maintenance burden?**

Parsing a binary AST format will not be as complicated as parsing existing JavaScript. It will undoubtedly increase complexity of implementation, but we believe is worth the cost given the trajectory of the complexity and size of web applications.

**Won't an AST format bloat file size?**

In our prototype described above, we found the AST format to be slightly smaller, even when comparing compressed sizes.

**How much of a performance win is there?**

For parsing desktop facebook.com, 10-15% of client-side CPU time is spent parsing JavaScript. The prototype we implemented reduced time to build the AST by 70-90%.

Additionally, many complex sites today fetch JavaScript on demand when a user interacts with some secondary functionality (e.g. when a user opens the Notifications panel on Facebook). Long parsing times on these interactive paths reduce the responsiveness of the site.

**Would it be possible to write a tool to convert serialized AST back to JS?**

Yes, it would be possible to automatically generate human-readable JavaScript from the AST format. By virtue of the syntax tree being abstract and not concrete, the pretty printer will not be a perfect recreation of the input JavaScript but it will be semantically equivalent.

Such serializers may be crucial to providing a compelling devtooling story.

**Is this made obsolete by programmable caching APIs?**

No. Programmable caching APIs drastically improve warm load times. This proposal improves cold load times. It composes nicely with caching APIs.

**Will this work alongside text JS source?**

Yes. HTML integration is forthcoming, but an application is free to mix loading text source and binary AST files.

**Will this be a new surface for attacks?**

Yes, but we believe this is a much smaller and much more controlled and testable surface than alternatives, such as bytecode.
