# Binary AST Tree Grammar Draft

The Binary AST Tree Grammar is isomorphic to ECMA-262 Syntactic Grammar. That is, an instance of a production of the binary AST grammar, or a Binary AST Node, corresponds to some Parse Node in the ECMA-262 syntactic grammar. This document specifies the following things.

1. The tree grammar itself described in IDL and its well-formedness.
1. The homomorphism *Ecmaify* from Binary AST Nodes to Parse Nodes by case analysis.
1. The similarity relation *~* between Parse Nodes and Parse Nodes.

A compliant Binary AST Encoder, for a source text that matches either the *Script* or *Module* goal symbol to produce a Parse Node *N*, a compliant Encoder must produce a Binary AST Node *B* such that *Ecmaify(B) ~ N*.

The inverse, *Ecmaify<sup>-1</sup>*, is not specified here. The inverse transform is the encoding of Parse Nodes. For *B* and *N* as above, for all *N'* such that *N' ~ N*, *Ecmaify<sup>-1</sup>(N') = B*.

## Tree Grammar

This section is derived from [Shift AST Spec](https://github.com/shapesecurity/shift-spec/blob/es2017/spec.idl) and parts are Copyright 2014 Shape Security, Inc.

Unlike the Shift AST spec, interface types are not used to inherit fields to control over ordering of fields. Type hierarchies that are not used to discriminate are collapsed to make space costs simple. Nor are they used to discriminate types, for which explicitly discriminated unions types are used.

NOTE: Whereas Shift AST's design principle is ease of search-and-replace of node types, Binary AST's design principle is ease of verification and ease of associating different behaviors with syntactically different (but possibly lexically similar) productions.

The grammar is WebIDL-like with the `[TypeIndicator]` and `NonEmpty` extensions per Shift AST spec. The `typedefs` of `or` types are to be read as recursive sum types.

```webidl
// Type aliases and enums.

typedef FrozenArray<(SpreadElement or Expression)> Arguments;
typedef DOMString string;
typedef string Identifier;
typedef string IdentifierName;
typedef string Label;

enum VariableDeclarationKind {
  "var",
  "let",
  "const"
};

enum CompoundAssignmentOperator {
  "+=",
  "-=",
  "*=",
  "/=",
  "%=",
  "**=",
  "<<=",
  ">>=",
  ">>>=",
  "|=",
  "^=",
  "&="
};

enum BinaryOperator {
  ",",
  "||",
  "&&",
  "|",
  "^",
  "&",
  "==",
  "!=",
  "===",
  "!==",
  "<",
  "<=",
  ">",
  ">=",
  "in",
  "instanceof",
  "<<",
  ">>",
  ">>>",
  "+",
  "-",
  "*",
  "/",
  "%",
  "**",
};

enum UnaryOperator {
  "+",
  "-",
  "!",
  "~",
  "typeof",
  "void",
  "delete"
};

enum UpdateOperator {
  "++",
  "--"
};


// Binary AST delayed assertions

interface AssertedScope {
  attribute FrozenArray<IdentifierName> varDeclaredNames;
  attribute FrozenArray<IdentifierName> lexicallyDeclaredNames;
  attribute FrozenArray<IdentifierName> capturedNames;
  attribute boolean hasDirectEval;
};


// nodes

interface Node {
  [TypeIndicator] readonly attribute Type type;
};

typedef (Script or Module) Program;

typedef (DoWhileStatement or
         ForInStatement or
         ForOfStatement or
         ForStatement or
         WhileStatement)
        IterationStatement;

typedef (Block or
         BreakStatement or
         ContinueStatement or
         ClassDeclaration or
         DebuggerStatement or
         EmptyStatement or
         ExpressionStatement or
         FunctionDeclaration or
         IfStatement or
         IterationStatement or
         LabelledStatement or
         ReturnStatement or
         SwitchStatement or
         SwitchStatementWithDefault or
         ThrowStatement or
         TryCatchStatement or
         TryFinallyStatement or
         VariableDeclaration or
         WithStatement)
        Statement;

typedef (LiteralBooleanExpression or
         LiteralInfinityExpression or
         LiteralNullExpression or
         LiteralNumericExpression or
         LiteralStringExpression)
        Literal;

typedef (Literal or
         LiteralRegExpExpression or
         ArrayExpression or
         ArrowExpression or
         AssignmentExpression or
         BinaryExpression or
         CallExpression or
         CompoundAssignmentExpression or
         ComputedMemberExpression or
         ConditionalExpression or
         ClassExpression or
         FunctionExpression or
         IdentifierExpression or
         NewExpression or
         NewTargetExpression or
         ObjectExpression or
         UnaryExpression or
         StaticMemberExpression or
         TemplateExpression or
         ThisExpression or
         UpdateExpression or
         YieldExpression or
         YieldStarExpression or
         AwaitExpression)
        Expression;

typedef (ComputedPropertyName or
         LiteralPropertyName)
        PropertyName;

typedef (Method or Getter or Setter) MethodDefinition;

typedef (MethodDefinition or
         DataProperty or
         ShorthandProperty)
        ObjectProperty;

typedef (ExportAllFrom or
         ExportFrom or
         ExportLocals or
         ExportDefault or
         Export)
        ExportDeclaration;

typedef (ImportNamespace or Import) ImportDeclaration;


// bindings

interface BindingIdentifier : Node {
  attribute Identifier name;
};

typedef (ObjectBinding or
         ArrayBinding)
        BindingPattern;
typedef (BindingPattern or
         BindingIdentifier)
        Binding;

typedef (AssignmentTargetIdentifier or
         ComputedMemberAssignmentTarget or
         StaticMemberAssignmentTarget)
        SimpleAssignmentTarget;
typedef (ObjectAssignmentTarget or
         ArrayAssignmentTarget)
        AssignmentTargetPattern;
// `DestructuringAssignmentTarget`
typedef (AssignmentTargetPattern or
         SimpleAssignmentTarget)
        AssignmentTarget;

// `FormalParameter`
typedef (Binding or
         BindingWithDefault)
        Parameter;

interface BindingWithDefault : Node {
  attribute Binding binding;
  attribute Expression init;
};

interface AssignmentTargetIdentifier : Node {
  attribute Identifier name;
};

interface ComputedMemberAssignmentTarget : Node {
  // The object whose property is being assigned.
  attribute (Expression or Super) _object;
  // The expression resolving to the name of the property to be accessed.
  attribute Expression expression;
};

interface StaticMemberAssignmentTarget : Node {
  // The object whose property is being assigned.
  attribute (Expression or Super) _object;
  // The name of the property to be accessed.
  attribute IdentifierName property;
};

// `ArrayBindingPattern`
interface ArrayBinding : Node {
  // The elements of the array pattern; a null value represents an elision.
  attribute FrozenArray<(Binding or BindingWithDefault)?> elements;
  attribute Binding? rest;
};

// `SingleNameBinding`
interface BindingPropertyIdentifier : Node {
  attribute BindingIdentifier binding;
  attribute Expression? init;
};

// `BindingProperty :: PropertyName : BindingElement`
interface BindingPropertyProperty : Node {
  attribute PropertyName name;
  attribute (Binding or BindingWithDefault) binding;
};

typedef (BindingPropertyIdentifier or
         BindingPropertyProperty)
        BindingProperty;

interface ObjectBinding : Node {
  attribute FrozenArray<BindingProperty> properties;
};

// This interface represents the case where the initializer is present in `AssignmentElement :: DestructuringAssignmentTarget Initializer_opt`.
interface AssignmentTargetWithDefault : Node {
  attribute AssignmentTarget binding;
  attribute Expression init;
};

// `ArrayAssignmentPattern`
interface ArrayAssignmentTarget : Node {
  // The elements of the array pattern; a null value represents an elision.
  attribute FrozenArray<(AssignmentTarget or AssignmentTargetWithDefault?)> elements;
  attribute AssignmentTarget? rest;
};

// `AssignmentProperty :: IdentifierReference Initializer_opt`
interface AssignmentTargetPropertyIdentifier : Node {
  attribute AssignmentTargetIdentifier binding;
  attribute Expression? init;
};

// `AssignmentProperty :: PropertyName : Node`
interface AssignmentTargetPropertyProperty : Node {
  attribute PropertyName name;
  attribute (AssignmentTarget or AssignmentTargetWithDefault) binding;
};

typedef (AssignmentTargetPropertyIdentifier or
         AssignmentTargetPropertyProperty)
        AssignmentTargetProperty;

// `ObjectAssignmentPattern`
interface ObjectAssignmentTarget : Node {
  attribute FrozenArray<AssignmentTargetProperty> properties;
};


// classes

interface ClassExpression : Node {
  attribute BindingIdentifier? name;
  attribute Expression? super;
  attribute FrozenArray<ClassElement> elements;
};

interface ClassDeclaration : Node {
  attribute BindingIdentifier name;
  attribute Expression? super;
  attribute FrozenArray<ClassElement> elements;
};

interface ClassElement : Node {
  // True iff `IsStatic` of ClassElement is true.
  attribute boolean isStatic;
  attribute MethodDefinition method;
};


// modules

interface Module : Node {
  attribute FrozenArray<Directive> directives;
  attribute FrozenArray<(ImportDeclaration or ExportDeclaration or Statement)> items;
};

// An `ImportDeclaration` not including a namespace import.
interface Import : Node {
  attribute string moduleSpecifier;
  // `ImportedDefaultBinding`, if present.
  attribute BindingIdentifier? defaultBinding;
  attribute FrozenArray<ImportSpecifier> namedImports;
};

// An `ImportDeclaration` including a namespace import.
interface ImportNamespace : Node {
  attribute string moduleSpecifier;
  // `ImportedDefaultBinding`, if present.
  attribute BindingIdentifier? defaultBinding;
  attribute BindingIdentifier namespaceBinding;
};

interface ImportSpecifier : Node {
  // The `IdentifierName` in the production `ImportSpecifier :: IdentifierName as ImportedBinding`; absent if this specifier represents the production `ImportSpecifier :: ImportedBinding`.
  attribute IdentifierName? name;
  attribute BindingIdentifier binding;
};

// `export * FromClause;`
interface ExportAllFrom : Node {
  attribute string moduleSpecifier;
};

// `export ExportClause FromClause;`
interface ExportFrom : Node {
  attribute FrozenArray<ExportFromSpecifier> namedExports;
  attribute string moduleSpecifier;
};

// `export ExportClause;`
interface ExportLocals : Node {
  attribute FrozenArray<ExportLocalSpecifier> namedExports;
};

// `export VariableStatement`, `export Declaration`
interface Export : Node {
  attribute (FunctionDeclaration or ClassDeclaration or VariableDeclaration) declaration;
};

// `export default HoistableDeclaration`, `export default ClassDeclaration`, `export default AssignmentExpression`
interface ExportDefault : Node {
  attribute (FunctionDeclaration or ClassDeclaration or Expression) body;
};

// `ExportSpecifier`, as part of an `ExportFrom`.
interface ExportFromSpecifier : Node {
  // The only `IdentifierName in `ExportSpecifier :: IdentifierName`, or the first in `ExportSpecifier :: IdentifierName as IdentifierName`.
  attribute IdentifierName name;
  // The second `IdentifierName` in `ExportSpecifier :: IdentifierName as IdentifierName`, if that is the production represented.
  attribute IdentifierName? exportedName;
};

// `ExportSpecifier`, as part of an `ExportLocals`.
interface ExportLocalSpecifier : Node {
  // The only `IdentifierName in `ExportSpecifier :: IdentifierName`, or the first in `ExportSpecifier :: IdentifierName as IdentifierName`.
  attribute IdentifierExpression name;
  // The second `IdentifierName` in `ExportSpecifier :: IdentifierName as IdentifierName`, if present.
  attribute IdentifierName? exportedName;
};


// property definition

// `MethodDefinition :: PropertyName ( UniqueFormalParameters ) { FunctionBody }`, `GeneratorMethod :: * PropertyName ( UniqueFormalParameters ) { GeneratorBody }`, `AsyncMethod :: async PropertyName ( UniqueFormalParameters ) { AsyncFunctionBody }`
interface Method : Node {
  // True for `AsyncMethod`, false otherwise.
  attribute boolean isAsync;
  // True for `GeneratorMethod`, false otherwise.
  attribute boolean isGenerator;
  // The `UniqueFormalParameters`.
  attribute PropertyName name;
  attribute FormalParameters params;
  attribute FunctionBody body;
};

// `get PropertyName ( ) { FunctionBody }`
interface Getter : Node {
  attribute PropertyName name;
  attribute FunctionBody body;
};

// `set PropertyName ( PropertySetParameterList ) { FunctionBody }`
interface Setter : Node {
  attribute PropertyName name;
  // The `PropertySetParameterList`.
  attribute Parameter param;
  attribute FunctionBody body;
};

// `PropertyDefinition :: PropertyName : AssignmentExpression`
interface DataProperty : Node {
  attribute PropertyName name;
  // The `AssignmentExpression`.
  attribute Expression expression;
};

// `PropertyDefinition :: IdentifierReference`
interface ShorthandProperty : Node {
  // The `IdentifierReference`.
  attribute IdentifierExpression name;
};

interface ComputedPropertyName : Node {
  attribute Expression expression;
};

// `LiteralPropertyName`
interface StaticPropertyName : Node {
  attribute string value;
};


// literals

// `BooleanLiteral`
interface LiteralBooleanExpression : Node {
  attribute boolean value;
};

// A `NumericLiteral` for which the Number value of its MV is positive infinity.
interface LiteralInfinityExpression : Node { };

// `NullLiteral`
interface LiteralNullExpression : Node { };

// `NumericLiteral`
interface LiteralNumericExpression : Node {
  attribute double value;
};

// `RegularExpressionLiteral`
interface LiteralRegExpExpression : Node {
  attribute string pattern;
  attribute string flags;
};

// `StringLiteral`
interface LiteralStringExpression : Node {
  attribute string value;
};


// other expressions

// `ArrayLiteral`
interface ArrayExpression : Node {
  // The elements of the array literal; a null value represents an elision.
  attribute FrozenArray<(SpreadElement or Expression)?> elements;
};

// `ArrowFunction`, `AsyncArrowFunction`
interface ArrowExpression : Node {
  // True for `AsyncArrowFunction`, false otherwise.
  attribute boolean isAsync;
  attribute FormalParameters params;
  attribute (FunctionBody or Expression) body;
};

// `AssignmentExpression :: LeftHandSideExpression = AssignmentExpression`
interface AssignmentExpression : Node {
  // The `LeftHandSideExpression`.
  attribute AssignmentTarget binding;
  // The `AssignmentExpression` following the `=`.
  attribute Expression expression;
};

// `ExponentiationExpression`, `MultiplicativeExpression`, `AdditiveExpression`, `ShiftExpression`, `RelationalExpression`, `EqualityExpression`, `BitwiseANDExpression`, `BitwiseXORExpression`, `BitwiseORExpression`, `LogicalANDExpression`, `LogicalORExpression`
interface BinaryExpression : Node {
  attribute BinaryOperator operator;
  // The expression before the operator.
  attribute Expression left;
  // The expression after the operator.
  attribute Expression right;
};

interface CallExpression : Node {
  attribute (Expression or Super) callee;
  attribute Arguments arguments;
};

// `AssignmentExpression :: LeftHandSideExpression AssignmentOperator AssignmentExpression`
interface CompoundAssignmentExpression : Node {
  attribute CompoundAssignmentOperator operator;
  // The `LeftHandSideExpression`.
  attribute SimpleAssignmentTarget binding;
  // The `AssignmentExpression`.
  attribute Expression expression;
};

interface ComputedMemberExpression : Node {
  // The object whose property is being accessed.
  attribute (Expression or Super) _object;
  // The expression resolving to the name of the property to be accessed.
  attribute Expression expression;
};

// `ConditionalExpression :: LogicalORExpression ? AssignmentExpression : AssignmentExpression`
interface ConditionalExpression : Node {
  // The `LogicalORExpression`.
  attribute Expression test;
  // The first `AssignmentExpression`.
  attribute Expression consequent;
  // The second `AssignmentExpression`.
  attribute Expression alternate;
};

// `FunctionExpression`,
// `GeneratorExpression`,
// `AsyncFunctionExpression`,
interface FunctionExpression : Node {
  attribute boolean isAsync;
  attribute boolean isGenerator;
  attribute AssertedScope? scope;
  attribute BindingIdentifier? name;
  attribute FormalParameters params;
  attribute FunctionBody body;
};

// `IdentifierReference`
interface IdentifierExpression : Node {
  attribute Identifier name;
};

interface NewExpression : Node {
  attribute Expression callee;
  attribute Arguments arguments;
};

interface NewTargetExpression : Node { };

interface ObjectExpression : Node {
  attribute FrozenArray<ObjectProperty> properties;
};

interface UnaryExpression : Node {
  attribute UnaryOperator operator;
  attribute Expression operand;
};

interface StaticMemberExpression : Node {
  // The object whose property is being accessed.
  attribute (Expression or Super) _object;
  // The name of the property to be accessed.
  attribute IdentifierName property;
};

// `TemplateLiteral`, `MemberExpression :: MemberExpression TemplateLiteral`, `CallExpression : CallExpression TemplateLiteral`
interface TemplateExpression : Node {
  // The second `MemberExpression` or `CallExpression`, if present.
  attribute Expression? tag;
  // The contents of the template. This list must be alternating TemplateElements and Expressions, beginning and ending with TemplateElement.
  attribute FrozenArray<(Expression or TemplateElement)> elements;
};

// `PrimaryExpression :: this`
interface ThisExpression : Node { };

// `UpdateExpression :: LeftHandSideExpression ++`, `UpdateExpression :: LeftHandSideExpression --`, `UpdateExpression :: ++ LeftHandSideExpression`, ``UpdateExpression :: -- LeftHandSideExpression`
interface UpdateExpression : Node {
  // True for `UpdateExpression :: ++ LeftHandSideExpression` and `UpdateExpression :: -- LeftHandSideExpression`, false otherwise.
  attribute boolean isPrefix;
  attribute UpdateOperator operator;
  attribute SimpleAssignmentTarget operand;
};

// `YieldExpression :: yield`, `YieldExpression :: yield AssignmentExpression`
interface YieldExpression : Node {
  // The `AssignmentExpression`, if present.
  attribute Expression? expression;
};

// `YieldExpression :: yield * AssignmentExpression`
interface YieldStarExpression : Node {
  attribute Expression expression;
};

interface AwaitExpression : Node {
  attribute Expression expression;
};


// other statements

interface BreakStatement : Node {
  attribute Label? label;
};

interface ContinueStatement : Node {
  attribute Label? label;
};

interface DebuggerStatement : Node { };

interface DoWhileStatement : Node {
  attribute Expression test;
  attribute Statement body;
};

interface EmptyStatement : Node { };

interface ExpressionStatement : Node {
  attribute Expression expression;
};

interface ForInOfBinding : Node {
  attribute VariableDeclarationKind kind;
  attribute Binding binding;
};

// `for ( LeftHandSideExpression in Expression ) Statement`, `for ( var ForBinding in Expression ) Statement`, `for ( ForDeclaration in Expression ) Statement`, `for ( var BindingIdentifier Initializer in Expression ) Statement`
interface ForInStatement : Node {
  // The expression or declaration before `in`.
  attribute (ForInOfBinding or AssignmentTarget) left;
  // The expression after `in`.
  attribute Expression right;
  attribute Statement body;
};

// `for ( LeftHandSideExpression of Expression ) Statement`, `for ( var ForBinding of Expression ) Statement`, `for ( ForDeclaration of Expression ) Statement`
interface ForOfStatement : Node {
  // The expression or declaration before `of`.
  attribute (ForInOfBinding or AssignmentTarget) left;
  // The expression after `of`.
  attribute Expression right;
  attribute Statement body;
};

// `for ( Expression ; Expression ; Expression ) Statement`, `for ( var VariableDeclarationlist ; Expression ; Expression ) Statement`
interface ForStatement : Node {
  // The expression or declaration before the first `;`, if present.
  attribute (VariableDeclaration or Expression)? init;
  // The expression before the second `;`, if present
  attribute Expression? test;
  // The expression after the second `;`, if present
  attribute Expression? update;
  attribute Statement body;
};

// `if ( Expression ) Statement`, `if ( Expression ) Statement else Statement`,
interface IfStatement : Node {
  attribute Expression test;
  // The first `Statement`.
  attribute Statement consequent;
  // The second `Statement`, if present.
  attribute Statement? alternate;
};

interface LabelledStatement : Node {
  attribute Label label;
  attribute Statement body;
};

interface ReturnStatement : Node {
  attribute Expression? expression;
};

// A `SwitchStatement` whose `CaseBlock` is `CaseBlock :: { CaseClauses }`.
interface SwitchStatement : Node {
  attribute Expression discriminant;
  attribute FrozenArray<SwitchCase> cases;
};

// A `SwitchStatement` whose `CaseBlock` is `CaseBlock :: { CaseClauses DefaultClause CaseClauses }`.
interface SwitchStatementWithDefault : Node {
  attribute Expression discriminant;
  // The `CaseClauses` before the `DefaultClause`.
  attribute FrozenArray<SwitchCase> preDefaultCases;
  // The `DefaultClause`.
  attribute SwitchDefault defaultCase;
  // The `CaseClauses` after the `DefaultClause`.
  attribute FrozenArray<SwitchCase> postDefaultCases;
};

interface ThrowStatement : Node {
  attribute Expression expression;
};

// `TryStatement :: try Block Catch`
interface TryCatchStatement : Node {
  attribute Block body;
  attribute CatchClause catchClause;
};

// `TryStatement :: try Block Finally`, `TryStatement :: try Block Catch Finally`
interface TryFinallyStatement : Node {
  // The `Block`.
  attribute Block body;
  // The `Catch`, if present.
  attribute CatchClause? catchClause;
  // The `Finally`.
  attribute Block finalizer;
};

interface WhileStatement : Node {
  attribute Expression test;
  attribute Statement body;
};

interface WithStatement : Node {
  attribute Expression _object;
  attribute Statement body;
};


// other nodes

interface Block : Node {
  attribute AssertedScope? scope;
  attribute FrozenArray<Statement> statements;
};

// `Catch`
interface CatchClause : Node {
  attribute Binding binding;
  attribute Block body;
};

// An item in a `DirectivePrologue`
interface Directive : Node {
  attribute string rawValue;
};

interface FormalParameters : Node {
  attribute FrozenArray<Parameter> items;
  attribute Binding? rest;
};

interface FunctionBody : Node {
  attribute FrozenArray<Directive> directives;
  attribute FrozenArray<Statement> statements;
};

// `FunctionDeclaration`,
// `GeneratorDeclaration`,
// `AsyncFunctionDeclaration`
interface FunctionDeclaration : Node {
  attribute boolean isAsync;
  attribute boolean isGenerator;
  attribute AssertedScope? scope;
  attribute BindingIdentifier name;
  attribute FormalParameters params;
  attribute FunctionBody body;
};

interface Script : Node {
  attribute FrozenArray<Directive> directives;
  attribute FrozenArray<Statement> statements;
};

interface SpreadElement : Node {
  attribute Expression expression;
};

// `super`
interface Super : Node { };

// `CaseClause`
interface SwitchCase : Node {
  attribute Expression test;
  attribute FrozenArray<Statement> consequent;
};

// `DefaultClause`
interface SwitchDefault : Node {
  attribute FrozenArray<Statement> consequent;
};

// `TemplateCharacters`
interface TemplateElement : Node {
  attribute string rawValue;
};

interface VariableDeclaration : Node {
  attribute VariableDeclarationKind kind;
  [NonEmpty] attribute FrozenArray<VariableDeclarator> declarators;
};

interface VariableDeclarator : Node {
  attribute Binding binding;
  attribute Expression? init;
};
```

## *Ecmaify*

HasMalformedDirectives(_directives_ : Directive[], _nodes_ : Node[])

1. For each _node_ in _nodes_:
    1. If the _node_ is an ExpressionStatement and _node_.expression is a StringLiteralExpression:
        1. For each _dir_ in _directives_:
            1. If _node_.expression.value is the same value as _dir_.rawValue, then return **false**.
        1. NOTE: This restriction disallows encoding directives as string literals.
1. Return **true**.

StatementEcmaify(_stmt_ : Statement)

1. Let _n_ be Ecmaify(_stmt_).
1. If _stmt_ is a Block, then:
    1. Set _n_ to *BlockStatement* : _n_.
1. Else, if _stmt_ is IterationStatement, then:
    1. Set _n_ to *BreakableStatement* : _n_.
1. Else, return *Statement* : _n_.

StatementListEcmaify(_stmts_ : Statement[])

1. Let _list_ be an empty Parse Node.
1. If _stmts_ has length 0, then:
    1. Let _emptyStmt_ be *StatementListItem* : *EmptyStatement*.
    1. Tag _emptyStmt_ as being produced by _f_.
    1. Set _list_ to *StatementList* : _emptyStmt_.
1. For each _stmt_ in _stmts_:
    1. If _stmt_ is a FunctionDeclaration, then:
        1. Set _n_ to *HoistableDeclaration*: Ecmaify(_stmt_).
        1. Set _n_ to be *Declaration* : _n_.
    1. Else, set _n_ to be StatementEcmaify(_stmt_).
    1. Set _n_ to be *StatementListItem* : _n_.
    1. If _list_ is empty, then set _list_ to *StatementList* : _n_.
    1. Else, set _list_ to *StatementList* : _list_ _n_.
1. Return _list_.

VariableDeclarationListEcmaify(_decls_ : VariableDeclarator[])

1. Assert: _decls_ does not have length 0.
1. Let _list_ be an empty Parse Node.
1. For each _decl_ in _decls_:
    1. Let _n_ be an empty Parse Node.
    1. Let _binding_ be Ecmaify(_decl_.binding).
    1. If _decl_.init is null, then:
        1. If _decl_.binding is not BindingIdentifier, throw a **SyntaxError** exception.
        1. Set _n_ to *VariableDeclaration* : _binding_.
    1. Else:
        1. Let _init_ be Ecmaify(_decl_.init).
        1. Set _n_ to *VariableDeclaration* : _binding_ _init_.
    1. If _list_ is empty, set _list_ to *VariableDeclarationList* : _n_.
    1. Else, set _list_ to *VariableDeclarationList* : _list_ _n_.
1. Return _list_.

BindingListEcmaify(_decls_ : VariableDeclarator[])

1. Assert: _decls_ does not have length 0.
1. Let _list_ be an empty Parse Node.
1. For each _decl_ in _decls_:
    1. Let _n_ be an empty Parse Node.
    1. Let _binding_ be Ecmaify(_decl_.binding).
    1. If _decl_.init is null, then:
        1. If _decl_.binding is not BindingIdentifier, throw a **SyntaxError** exception.
        1. Set _n_ to *LexicalBinding* : _binding_.
    1. Else:
        1. Let _init_ be Ecmaify(_decl_.init).
        1. Set _n_ to *LexicalBinding* : _binding_ _init_.
    1. If _list_ is empty, set _list_ to *BindingList* : _n_.
    1. Else, set _list_ to *BindingList* : _list_ _n_.
1. Return _list_.

ElementListEcmaify(_elems_ : (SpreadElement or Expression)[])

1. Assert: _elems_ does not have length 0.
1. Let _list_ be an empty Parse Node.
1. Let _accumulatedElisions_ be an empty Parse Node.
1. For each _elem_ in _elems_:
    1. Let _n_ be an empty Parse Node.
    1. If _elem_ is null, then:
        1. If _accumulatedElisions_ is empty, then set _accumulatedElisions_ to *Elision* : **<tt>,</tt>**.
        1. Else set _accumulatedElisions_ to *Elision* : _accumulatedElisions_ **<tt>,</tt>**.
    1. Else:
        1. If _elem_ is a SpreadElement, then:
            1. Let _expr_ be AssigmentExpressionEcmaify(_elem_.expression).
            1. Set _n_ to *SpreadElement* : **<tt>...</tt>** _expr_.
        1. Else set _n_ to AssignmentExpressionEcmaify(_elem_).
        1. If _list_ is empty, then:
            1. If _accumulatedElisions_ is empty, then set _list_ to *ElementList* : _n_.
            1. Else set _list_ to *ElementList* : _accumulatedElisions_ _n_.
        1. Else:
            1. If _accumulatedElisions_ is empty, then set _list_ to *ElementList* : _list_ **<tt>,</tt>** _n_.
            1. Else set _list_ to *ElementList* : _list_ **<tt>,</tt>** _accumulatedElisions_ _n_.
        1. Set _accumulatedElisions_ to an empty Parse Node.
1. Return _list_ and _accumulatedElisions_.

FormalParameterListEcmaify(_params_ : Parameter[])

1. Assert: _params_ does not have length 0.
1. Let _list_ be an empty Parse Node.
1. For each _param_ in _params_:
    1. Let _n_ be Ecmaify(_param_).
    1. If _list_ is empty, then set _list_ to *FormalParameterList* : _n_.
    1. Else, set _list_ to *FormalParameterList* : _list_ **<tt>,</tt>** _n_.
1. Return _list_.

ArgumentListEcmaify(_args_ : Arguments)

1. Assert: _args_ does not have length 0.
1. Let _list_ be an empty Parse Node.
1. For each _arg_ in _args_:
    1. If _arg_ is a SpreadElement, then:
        1. Let _expr_ be AssignmentExpressionEcmaify(_arg_.expression).
        1. If _list_ is empty, then set _list_ to *ArgumentList* : **<tt>...</tt>** _expr_.
        1. Else, set _list_ to *ArgumentList* : _list_ **<tt>,</tt>** _expr_.
    1. Else:
        1. Let _expr_ be AssignmentExpressionEcmaify(_arg_).
        1. If _list_ is empty, then set _list_ to *ArgumentList* : _expr_.
        1. Else, set _list_ to *ArgumentList* : _list_ **<tt>,</tt>** _expr_.
1. Return _list_.

PropertyDefinitionListEcmaify(_props_ : ObjectProperty[])

1. Assert: _props_ does not have length 0.
1. Let _list_ be an empty Parse Node.
1. For each _prop_ in _props_:
    1. Let _n_ be an empty Parse Node.
    1. If _prop_ is a MethodDefinition, then set _n_ to *PropertyDefinition* : Ecmaify(_prop_).
    1. Else, set _n_ to Ecmaify(_prop_).
    1. If _list_ is empty, then set _list_ to *PropertyDefinitionList* : _n_.
    1. Else, set _list_ to *PropertyDefinitionList* : _list_ **<tt>,</tt>** _n_.
1. Return _list_.

CaseClauseEcmaify(_case_ : SwitchCase)

1. Let _expr_ be ExpressionEcmaify(_case_.test).
1. Let _clause_ be an empty Parse Node.
1. If _case_.consequent has length 0, then:
    1. Set _clause_ to *CaseClause* : **<tt>case</tt>** _expr_ **<tt>:</tt>**
1. Else:
    1. Let _stmts_ be StatementListEcmaify(_case_.consequent).
    1. Set _clause_ to *CaseClause* : **<tt>case</tt>** _expr_ **<tt>:</tt>** _stmts_
1. Return _clause_.

DefaultClauseEcmaify(_case_ : SwitchDefault)

1. Let _clause_ be an empty Parse Node.
1. If _case_.consequent has length 0, then:
    1. Set _clause_ to *CaseClause* : **<tt>default</tt>** **<tt>:</tt>**
1. Else:
    1. Let _stmts_ be StatementListEcmaify(_case_.consequent).
    1. Set _clause_ to *CaseClause* : **<tt>default</tt>** **<tt>:</tt>** _stmts_
1. Return _clause_.

CaseClausesEcmaify(_cases_ : SwitchCase[])

1. Assert: _cases_ does not have length 0.
1. Let _clauses_ be an empty Parse Node.
1. For _case_ in _cases_:
    1. Let _clause_ be CaseClauseEcmaify(_case_).
    1. If _clauses_ is empty, then set _clauses_ to *CaseClauses* : _clause_.
    1. Else, set _clauses_ to *CaseClauses* : _clauses_ _clause_.
1. Return _clauses_.

ArgumentsEcmaify(_args_ : Arguments)

1. If _args_ has length 0, then return *Arguments* : **<tt>(</tt>** **<tt>)</tt>**.
1. Else:
    1. Let _list_ be ArgumentListEcmaify(_call_.arguments).
    1. Return *Arguments* : **<tt>(</tt>** _list_ **<tt>)</tt>**.

PrimaryExpressionEcmaify(_e_ : Expression)

1. If _e_ is a ThisExpression, then return Ecmaify(_e_).
1. Else if _pn_ is an IdentifierExpression, a Literal, an ArrayExpression, an ObjectExpression, a FunctionExpression, a ClassExpression, a LiteralRegExpExpression, or a TemplateExpression, then return *PrimaryExpression* : Ecmaify(_e_).
1. Else:
    1. Let _parenthesized_ be *ParenthesizedExpression* : **<tt>(</tt>** ExpressionEcmaify(_e_) **<tt>)</tt>**.
    1. Return *PrimaryExpression* : _parenthesized_.

MemberExpressionEcmaify(_e_ : Expression)

1. If _e_ is a ComputedMemberExpression, a StaticMemberExpression, a NewTargetExpression, or a NewExpression, then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be PrimaryExpressionEcmaify(_e_).
    1. Return *MemberExpression* : _n_.

LeftHandSideExpressionEcmaify(_e_ : Expression)

1. If _e_ is a NewExpression, then:
    1. Let _n_ be *NewExpression* : Ecmaify(_e_).
    1. Return *LeftHandSideExpression* : _n_.
1. Else, if _e_ is a CallExpression, then return *LeftHandExpression* : Ecmaify(_e_).
1. Else:
    1. Let _n_ be *NewExpression* : MemberExpressionEcmaify(_e_).
    1. Return *LeftHandSideExpression*: _n_.

UpdateExpressionEcmaify(_e_ : Expression)

1. If _e_ is an UpdateExpression, then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be LeftHandSideExpressionEcmaify(_e_).
    1. Return *UpdateExpression* : _n_.

UnaryExpressionEcmaify(_e_ : Expression)

1. If _e_ is a UnaryExpression, then return Ecmaify(_e_).
1. Else, if _e_ is an AwaitExpression, then:
    1. Let _n_ be Ecmaify(_e_).
    1. Return *UnaryExpression* : _n_.
1. Else:
    1. Let _n_ be UpdateExpressionEcmaify(_e_).
    1. Return *UnaryExpresison* : _n_.

ExponentiationExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "**", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be UnaryExpressionEcmaify(_e_).
    1. Return *ExponentiationExpression* : _n_.

MultiplicativeExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "*", "/", or "%", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be ExponentiationExpressionEcmaify(_e_).
    1. Return *MultiplicativeExpression* : _n_.

AdditiveExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "+", or "-", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be MultiplicativeExpressionEcmaify(_e_).
    1. Return *AdditiveExpression* : _n_.

ShiftExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "<<", ">>", or ">>>", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be AdditiveExpressionEcmaify(_e_).
    1. Return *ShiftExpression* : _n_.

RelationalExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "<", ">", "<=", ">=", "instanceof", or "in", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be ShiftExpressionEcmaify(_e_).
    1. Return *RelationalExpression* : _n_.

EqualityExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "==", "!=", "===", or "!==", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be RelationalExpressionEcmaify(_e_).
    1. Return *EqualityExpression* : _n_.

BitwiseANDExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "&", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be EqualityExpressionEcmaify(_e_).
    1. Return *BitwiseANDExpression* : _n_.

BitwiseXORExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "^", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be BitwiseANDExpressionEcmaify(_e_).
    1. Return *BitwiseXORExpression* : _n_.

BitwiseORExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "|", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be BitwiseXORExpressionEcmaify(_e_).
    1. Return *BitwiseORExpression* : _n_.

LogicalANDExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "&&", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be BitwiseORExpressionEcmaify(_e_).
    1. Return *LogicalANDExpression* : _n_.

LogicalORExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is "||", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be LogicalANDExpressionEcmaify(_e_).
    1. Return *LogicalORExpression* : _n_.

ConditionalExpressionEcmaify(_e_ : Expression)

1. If _e_ is a ConditionalExpression, then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be LogicalORExpressionEcmaify(_e_).
    1. Return *ConditionalExpression* : _n_.

AssignmentExpressionEcmaify(_e_ : Expression)

1. If _e_ is a YieldExpression, an ArrowExpression, an AssignmentExpression, or a CompoundAssignmentExpression, then return *AssignmentExpression* : Ecmaify(_e_).
1. Else:
    1. Let _n_ be ConditionalExpressionEcmaify(_e_).
    1. Return *AssignmentExpression* : _n_.

ExpressionEcmaify(_e_ : Expression)

1. If _e_ is a BinaryExpression and _e_.operator is ",", then return Ecmaify(_e_).
1. Else:
    1. Let _n_ be AssignmentExpressionEcmaify(_e_).
    1. Return *Expression* : _n_.

LazyFunctionBodyEcmaify(_f_ : (FunctionBody or Expression))

1. NOTE: When an *EmptyStatement* tagged as being produced by a Binary AST FunctionBody is evaluated at runtime, Ecmaify is run on the function at that time.
1. Let _emptyStmt_ be *StatementListItem* : *EmptyStatement*.
1. Tag _emptyStmt_ as being produced by _f_.
1. Let _stmts_ *StatementList* : _emptyStmt_.
1. Return *FunctionBody* : _stmts_.

// Ecmaify : Program -> Parse Node

Ecmaify(_s_ : Script)

1. If HasMalformedDirectives(_s_.directives, _s_.statements) is **false**, throw a **SyntaxError** exception.
1. Let _dirsStmts_ be _s_.directives prepended to _s_.statements.
1. If _s_.directives_ has length 0 and _s_.statements has length 0, then:
    1. return *Script* :
1. Else:
    1. Let _dirStmts_ be _s_.directives concatenated with _s_.statements.
    1. Let _stmts_ be StatementListEcmaify(_dirStmts_).
    1. Return *Script* : _stmts_.

Ecmaify(m : Module)

TODO

// Ecmaify : (FunctionBody or Expression) -> Parse Node

Ecmaify(_f_ : (FunctionBody or Expression))

1. NOTE: This is called when an *EmptyStatement* tagged as being produced by a Binary AST FunctionBody or Expression is evaluated at runtime.
1. If _f_ is a FunctionBody, then:
    1. If HasMalformedDirectives(_f_.directives, _f_.statements) is **false**, throw a **SyntaxError** exception.
    1. Let _dirStmts_ be _f_.directives concatenated with _f_.statements.
    1. Let _stmts_ be StatementListEcmaify(_dirStmts_).
    1. Return *FunctionBody* : _stmts_.
1. Else return ExpressionEcmaify(_f_).

// Ecmaify : Statement -> Parse Node

Ecmaify(_b_ : Block)

1. Let _stmts_ be StatementListEcmaify(_b_.statements).
1. Return *Block* : **<tt>{</tt>** _stmts_ **<tt>}</tt>**.

Ecmaify(_brk_ : BreakStatement)

1. If _brk_.label is null, then return *BreakStatement* : **<tt>break</tt>** **<tt>;</tt>**.
1. Else, return *BreakStatement* : **<tt>break</tt>** _brk_.label **<tt>;</tt>**.

Ecmaify(_cont_ : ContinueStatement)

1. If _cont_.label is null, then:
    1. Return *ContinueStatement* : **<tt>continue</tt>** **<tt>;</tt>**.
1. Else, return *ContinueStatement* : **<tt>continue</tt>** _cont_.label **<tt>;</tt>**.

Ecmaify(_cls_ : ClassDeclaration)

TODO

Ecmaify(_dbg_ : DebuggerStatement)

1. Return *DebuggerStatement* : **<tt>debugger</tt>** **<tt>;</tt>**.

Ecmaify(_e_ : EmptyStatement)

1. Return *EmptyStatement* : **<tt>;</tt>**.

Ecmaify(_expr_ : ExpressionStatement)

1. Return *ExpressionStatement* : Ecmaify(_expr_.expression) **<tt>;</tt>**.

Ecmaify(_func_ : FunctionDeclaration)

1. If _func_.isAsync is *true*, then:
    1. Let _body_ be *AsyncFunctionBody* : LazyFunctionBodyEcmaify(_func_.body).
    1. Return *AsyncFunctionDeclaration* : **<tt>async</tt>** **<tt>function</tt>** _func_.name **<tt>(</tt>** Ecmaify(params) **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.
1. Else, if _func_.isGenerator is *true*, then:
    1. Let _body_ be *GeneratorBody* : LazyFunctionBodyEcmaify(_func_.body).
    1. Return *GeneratorDeclaration* : **<tt>function</tt>** __<tt>*</tt>__ _func_.name **<tt>(</tt>** Ecmaify(params) **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.
1. Else:
    1. Let _body_ be LazyFunctionBodyEcmaify(_func_.body).
    1. Return *FunctionDeclaration* : **<tt>function</tt>** _func_.name **<tt>(</tt>** Ecmaify(params) **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.

Ecmaify(_if_ : IfStatement)

1. Let _test_ be Ecmaify(_if_.test).
1. Let _then_ be Ecmaify(_if_.consequent).
1. If _if_.alternate is null, then:
    1. Return *IfStatement* : **<tt>if</tt>** **<tt>(</tt>** _test_ **<tt>)</tt>** _then_.
1. Else, return *IfStatement* : **<tt>if</tt>** **<tt>(</tt>** _test_ **<tt>)</tt>** _then_ **else** StatementEcmaify(_if_.alternate).

Ecmaify(_doWhile_ : DoWhileStatement)

1. Let _test_ be ExpressionEcmaify(_doWhile_.test).
1. Let _body_ be StatementEcmaify(_doWhile_.body).
1. Return *IterationStatement* : **<tt>do</tt>** _body_ **<tt>while</tt>** **<tt>(</tt>** _test_ **<tt>)</tt>** **<tt>;</tt>**.

Ecmaify(_forIn_ : ForInStatement)

1. Let _left_ be an empty Parse Node.
1. If _forIn_.left is a ForInOfBinding, then set _left_ to Ecmaify(_forIn_.left).
1. Else, set _left_ to LeftHandSideExpressionEcmaify(_forIn_.left).
1. Let _right_ be Ecmaify(_forIn_.right).
1. Let _body_ be StatementEcmaify(_forIn_.body).
1. Return *IterationStatement* : **<tt>for</tt>** **<tt>(</tt>** _left_ **in** _right_ **<tt>)</tt>** _body_.

Ecmaify(_forOf_ : ForOfStatement)

1. Let _left_ be an empty Parse Node.
1. If _forOf_.left is a ForInOfBinding, then set _left_ to Ecmaify(_forOf_.left).
1. Else, set _left_ to LeftHandSideExpressionEcmaify(_forOf_.left).
1. Let _right_ be Ecmaify(_forOf_.right).
1. Let _body_ be StatementEcmaify(_forOf_.body).
1. Return *IterationStatement* : **<tt>for</tt>** **<tt>(</tt>** _left_ **of** _right_ **<tt>)</tt>** _body_.

Ecmaify(_for_ : ForStatement)

1. Let _init_, _test_, and _update_ be empty Parse Nodes.
1. If _for_.init is not null, then:
    1. If _for_.init is a VariableDeclaration, then:
        1. If _for_.init.kind is "var", then set _init_ to the partial Parse Node **<tt>var</tt>** VariableDeclarationEcmaify(_for_.init.declarators).
        1. Else set _init_ to *LexicalDeclaration* : Ecmaify(_for_.init.kind) BindingListEcmaify(_for_.init.declarators).
    1. Else set _init_ to LeftHandSideExpressionEcmaify(_for_.init).
1. If _for_.test is not null, then set _test_ to ExpressionEcmaify(_for_.test).
1. If _for_.update is not null, then set _update_ to ExperssionEcmaify(_for_.test).
1. Return *IterationStatement* : **<tt>for</tt>** **<tt>(</tt>** _init_ **<tt>;</tt>** _test_ **<tt>;</tt>** _update_ **<tt>)</tt>** StatementEcmaify(_for_.body).

Ecmaify(_while_ : WhileStatement)

1. Let _test_ be ExpressionEcmaify(_while_.test).
1. Let _body_ be StatementEcmaify(_while_.body).
1. Return *IterationStatement* : **<tt>while</tt>** **<tt>(</tt>** _test_ **<tt>)</tt>** _body_.

Ecmaify(_labelled_ : LabelledStatement)

1. Let _body_ be an empty Parse Node.
1. If _labelled_.body is a FunctionDeclaration, then:
    1. If _labelled_.body.isAsync is true or _labelled_.body.isGenerator is true, then throw a **SyntaxError** exception.
    1. Set _body_ to Ecmaify(_labelled_.body).
1. Else, set _body_ to StatementEcmaify(_labelled_.body).
1. Let _item_ be *LabelledItem* : _body_.
1. Return *LabelledStatement* : _labelled_.label **<tt>:</tt>** _item_.

Ecmaify(_ret_ : ReturnStatement)

1. If _ret_.expression is null, then:
    1. Return *ReturnStatement* : **<tt>return</tt>** **<tt>;</tt>**.
1. Else, return *ReturnStatement* : **<tt>return</tt>** ExpressionEcmaify(_ret_.expression).

Ecmaify(_switch_ : SwitchStatement)

1. Let _expr_ be ExpressionEcmaify(_switch_.discriminant).
1. Let _block_ be an empty Parse Node.
1. If _switch_.cases has length 0, then set _block_ to *CaseBlock* : **<tt>{</tt>** **<tt>}</tt>**.
1. Else:
    1. Let _clauses_ be CaseClausesEcmaify(_switch_.cases).
    1. Set _block_ to *CaseBlock* : **<tt>{</tt>** _clauses_ **<tt>}</tt>**.
1. Return *SwitchStatement* : **<tt>switch</tt>** **<tt>(</tt>** _expr_ **<tt>)</tt>** _block_.

Ecmaify(_switch_ : SwitchStatementWithDefault)

1. Let _expr_ be ExpressionEcmaify(_switch_.discriminant).
1. Let _defaultClause_ be DefaultClauseEcmaify(_switch_.defaultCase).
1. Let _block_ be an empty Parse Node.
1. If _switch_.preDefaultCases has length 0, then:
    1. If _switch_.postDefaultCases has length 0, then:
        1. Set _block_ to *CaseBlock* : **<tt>{</tt>** _defaultClause_ **<tt>}</tt>**.
    1. Else:
        1. Let _postDefaultClauses_ be CaseClausesEcmaify(_switch_.postDefaultCases).
        1. Set _block_ to *CaseBlock* : **<tt>{</tt>** _defaultClause_ _postDefaultClauses_ **<tt>}</tt>**.
1. Else if _switch_.postDefaultCases has length 0, then:
    1. Let _preDefaultClauses_ be CaseClausesEcmaify(_switch_.preDefaultCases).
    1. Set _block_ to *CaseBlock* : **<tt>{</tt>** _preDefaultClauses_ _defaultClause_ **<tt>}</tt>**
1. Else:
    1. Let _preDefaultClauses_ be CaseClausesEcmaify(_switch_.preDefaultCases).
    1. Let _postDefaultClauses_ be CaseClausesEcmaify(_switch_.postDefaultCases).
    1. Set _block_ to *CaseBlock* : **<tt>{</tt>** _preDefaultClauses_ _defaultClause_ _postDefaultClauses_ **<tt>}</tt>**.
1. Return *SwitchStatement* : **<tt>switch</tt>** **<tt>(</tt>** _expr_ **<tt>)</tt>** _block_.

Ecmaify(_throw_ : ThrowStatement)

1. Return *ThrowStatement* : **<tt>throw</tt>** ExpressionEcmaify(throw.expression).

Ecmaify(_tryCatch_ : TryCatchStatement)

1. Let _block_ be Ecmaify(_tryCatch_.body).
1. Let _catch_ be Ecmaify(_tryCatch_.catchClause).
1. Return *TryStatement* : **<tt>try</tt>** _block_ _catch_.

Ecmaify(_tryFinally_ : TryFinallyStatement)

1. Let _block_ be Ecmaify(tryCatch.body).
1. Let _finally_ be *Finally* : **<tt>finally</tt>** Ecmaify(_tryFinally_.finalizer).
1. If _tryFinally_.catch is null, then:
    1. Return *TryStatement* : **<tt>try</tt>** _block_ _finally_.
1. Else:
    1. Let _catch_ be Ecmaify(tryCatch.catchClause).
    1. Return *TryStatement* : **<tt>try</tt>** _block_ _catch_ _finally_.

Ecmaify(_with_ : WithStatement)

1. Let _expr_ be ExpressionEcmaify(_with_._object).
1. Let _body_ be StatementEcmaify(_with_.body).
1. Return *WithStatement* : **<tt>with</tt>** **<tt>(</tt>** _expr_ **<tt>)</tt>** _body_.

// Ecmaify : Expression -> Parse Node

Ecmaify(_litBool_ : LiteralBooleanExpression)

1. Let _l_ be an empty Parse Node.
1. If _litBool_.value is **true**, then set _l_ to *BooleanLiteral* : **<tt>true</tt>**.
1. Else, set _l_ to *BooleanLiteral* : **<tt>false</tt>**.
1. Return *Literal* : _l_.

Ecmaify(_litInfty_ : LiteralInfinityExpresion)

1. NOTE: The full tree of numeric literals is elided here and left as an exercise to the reader.
1. NOTE: This node is not for the `Infinity` identifier, but for literal decimals whose mathematical values exceed the bounds of doubles.
1. Let _l0_ be *DecimalLiteral* : **<tt>2e308</tt>**.
1. Let _l1_ be *NumericLiteral* : _l0_.
1. Return *Literal* : _l1_.

Ecmaify(_litNull_ : LiteralNullExpression)

1. Let _l_ be *NullLiteral* : **<tt>null</tt>**.
1. Return *Literal* : _l_.

Ecmaify(_litNumeric_ : LiteralNumericExpression)

1. NOTE: The full tree of numeric literals is elided here and left as an exercise to the reader.
1. NOTE: The value in _litNumeric_ is positive, non-NaN, and finite.
1. Let _l_ be *DecimalLiteral* : _litNumeric_.value.
1. Return *Literal* : _l_.

Ecmaify(_litRegexp_ : LiteralRegExpExpression)

1. NOTE: The full tree of regular expression literals is elided here and left as an exercise to the reader.
1. Let _body_ be *RegularExpressionBody* : _litRegexp_.pattern.
1. Let _flags_ be *RegularExpressionFlags* : _litRegexp_.flags.
1. Return *RegularExpressionLiteral* : **<tt>/</tt>** _body_ **<tt>/</tt>** _flags_.

Ecmaify(_litStr_ : LiteralStringExpression)

1. Let _v_ be the value of _litStr_.value escaped for double quotes.
1. Let _l_ be *StringLiteral* : **<tt>&#34;</tt>** _v_ **<tt>&#34;</tt>**.
1. Return *Literal* : _l_.

Ecmaify(_litArray_ : ArrayExpression)

1. If _litArray_.elements has length 0, then:
    1. Return *ArrayLiteral* : **<tt>[</tt>** **<tt>]</tt>**.
1. Let and _elementList_ and _elisions_ be the results of ElementListEcmaify(_litArray_.elements).
1. If _elementList_ is empty:
    1. Assert: _elisions_ is not empty.
    1. Return *ArrayLiteral* : **<tt>[</tt>** _elisions_ **<tt>]</tt>**.
1. Else:
    1. If _elisions_ is empty, then return *ArrayLiteral* : **<tt>[</tt>** _elementList_ **<tt>]</tt>**.
    1. Else return *ArrayLiteral* : **<tt>[</tt>** _elementList_ **<tt>,</tt>** _elisions_ **<tt>]</tt>**.

Ecmaify(_litArrow_ : ArrowExpression)

1. NOTE: Arrow expressions are canonicalized to use parenthesized parameters and braced bodies.
1. Let _params0_ be *UniqueFormalParameters* : Ecmaify(params).
1. Let _params_ be *ArrowFormalParameters* : **<tt>(</tt>** _params0_ **<tt>(</tt>**.
1. Let _lazyBody_ be LazyFunctionBodyEcmaify(_litArrow_.body).
1. If _litArrow_.isAsync is **true**, then:
    1. Let _asyncHead_ be *AsyncArrowHead* : **<tt>async</tt>** _params_.
    1. Let _asyncBody0_ be *AsyncFunctionBody* : _lazyBody_.
    1. Let _asyncBody_ be *AsyncConciseBody* : **<tt>{</tt>** _asyncBody0_ **<tt>}</tt>**.
    1. Return *AsyncArrowFunction* : _asyncHead_ **<tt>=&gt;</tt>** _asyncBody_.
1. Else:
    1. Let _body_ be *ConciseBody* : **<tt>{</tt>** _lazyBody_ **<tt>}</tt>**.
    1. Return *ArrowFunction* : _params_ **<tt>=&gt;</tt>** _body_.

Ecmaify(_assign_ : AssignmentExpression)

1. Let _lhs_ be LeftHandSideExpressionEcmaify(_assign_.binding).
1. Let _rhs_ be AssignmentExpressionEcmaify(_assign_.expression).
1. Return *AssignmentExpression* : _lhs_ **<tt>=</tt>** _rhs_.

Ecmaify(_bin_ : BinaryExpression)

1. If _bin_.operator is ",", then:
    1. Let _left_ be ExpressionEcmaify(_bin_.left).
    1. Let _right_ be AssignmentExpressionEcmaify(_bin_.right).
    1. Return *Expression* : _left_ **<tt>,</tt>** _right_.
1. Else, if _bin_.operator is "||", then:
    1. Let _left_ be LogicalOREcmaify(_bin_.left).
    1. Let _right_ be LogicalANDEcmaify(_bin_.right).
    1. Return *LogicalORExpression* : _left_ **<tt>||</tt>** _right_.
1. Else, if _bin_.operator is "&&", then:
    1. Let _left_ be LogicalANDEcmaify(_bin_.left).
    1. Let _right_ be BitwiseOREcmaify(_bin_.right).
    1. Return *LogicalANDExpression* : _left_ **<tt>&&</tt>** _right_.
1. Else, if _bin_.operator is "|", then:
    1. Let _left_ be BitwiseOREcmaify(_bin_.left).
    1. Let _right_ be BitwiseXOREcmaify(_bin_.right).
    1. Return *BitwiseORExpression* : _left_ **<tt>|</tt>** _right_.
1. Else, if _bin_.operator is "^", then:
    1. Let _left_ be BitwiseXOREcmaify(_bin_.left).
    1. Let _right_ be BitwiseANDEcmaify(_bin_.right).
    1. Return *BitwiseXORExpression* : _left_ **<tt>^</tt>** _right_.
1. Else, if _bin_.operator is "&", then:
    1. Let _left_ be BitwiseANDEcmaify(_bin_.left).
    1. Let _right_ be EqualityExpressionEcmaify(_bin_.right).
    1. Return *BitwiseANDExpression* : _left_ **<tt>&</tt>** _right_.
1. Else, if _bin_.operator is "==", "!=", "===", or "!==", then:
    1. Let _left_ be EqualityExpressionEcmaify(_bin_.left).
    1. Let _right_ be RelationalExpressionEcmaify(_bin_.right).
    1. If _bin_.operator is "==", then return *EqualityExpression* : _left_ **<tt>==</tt>** _right_.
    1. Else, if _bin_.operator is "!=", then return *EqualityExpression* : _left_ **<tt>!=</tt>** _right_.
    1. Else, if _bin_.operator is "===", then return *EqualityExpression* : _left_ **<tt>===</tt>** _right_.
    1. Else return *EqualityExpression* : _left_ **<tt>!==</tt>** _right_.
1. Else, if _bin_.operator is "<", ">", "<=", ">=", "instanceof", or "in", then:
    1. Let _left_ be RelationExpressionEcmaify(_bin_.left).
    1. Let _right_ be ShiftExpressionEcmaify(_bin_.right).
    1. If _bin_.operator is "<" return *RelationalExpression* : _left_ **<tt>&lt;</tt>** _right_.
    1. Else, if _bin_.operator is ">", then return *RelationalExpression* : _left_ **<tt>&gt;</tt>** _right_.
    1. Else, if _bin_.operator is "<=" return *RelationalExpression* : _left_ **<tt>&lt;=</tt>** _right_.
    1. Else, if _bin_.operator is ">=", then return *RelationalExpression* : _left_ **<tt>&gt;=</tt>** _right_.
    1. Else, if _bin_.operator is "instanceof", then return *RelationalExpression* : _left_ **<tt>instanceof</tt>** _right_.
    1. Else return *RelationalExpression* : _left_ **<tt>in</tt>** _right_.
1. Else, if _bin_.operator is "<<", ">>", ">>>", then:
    1. Let _left_ be ShiftExpressionEcmaify(_bin_.left).
    1. Let _right_ be AdditiveExpressionEcmaify(_bin_.right).
    1. If _bin_.operator is "<<", then return *ShiftExpression* : _left_ **<tt>&lt;&lt;</tt>** _right_.
    1. Else, if _bin_.operator is ">>", then return *ShiftExpression* : _left_ **<tt>&gt;&gt;</tt>** _right_.
    1. Else return *ShiftExpression* : _left_ **<tt>&gt;&gt;&gt;</tt>** _right_.
1. Else, if _bin_.operator is "+", "-", then:
    1. Let _left_ be AdditiveExpressionEcmaify(_bin_.left).
    1. Let _right_ be MultiplicativeExpressionEcmaify(_bin_.right).
    1. If _bin_.operator is "+", then return *AdditiveExpression* : _left_ **<tt>+</tt>** _right_.
    1. Else return *AdditiveExpression* : _left_ **<tt>-</tt>** _right_.
1. Else, if _bin_.operator is "*", "/", or "%", then:
    1. Let _left_ be MultiplicativeExpressionEcmaify(_bin_.left).
    1. Let _right_ be ExponentationExpressionEcmaify(_bin_.right).
    1. Let _op_ be an empty Parse Node.
    1. If _bin_.operator is "*", then set _op_ to *MultiplicativeOperator* : __<tt>*</tt>__.
    1. If _bin_.operator is "/", then set _op_ to *MultiplicativeOperator* : **<tt>/</tt>**.
    1. If _bin_.operator is "%", then set _op_ to *MultiplicativeOperator* : **<tt>%</tt>**.
    1. Return *MultiplicativeExpression* : _left_ _op_ _right_.
1. Else:
    1. Let _left_ be UpdateExpressionEcmaify(_bin_.left).
    1. Let _right_ be ExponentationExpressionEcmaify(_bin_.right).
    1. Return *ExponentiationExpression* : _left_ **<tt>&#42;&#42;</tt>** _right_.

Ecmaify(_call_ : CallExpression)

1. Let _args_ be ArgumentsEcmaify(_call_.arguments).
1. If _call_.callee is Super, then:
    1. Let _superCall_ be *SuperCall* : **<tt>super</tt>** _args_.
    1. Return *CallExpression* : _superCall_.
1. Else:
    1. NOTE: CallExpressions are canonicalized to have the callee to always be parenthesized.
    1. Let _callee_ be MemberExpressionEcmaify(_call_.callee).
    1. Return *CallExpression* : _callee_ _args_.

Ecmaify(_assign_ : CompoundAssignmentExpression)

1. Let _lhs_ be LeftHandSideExpressionEcmaify(_assign_.binding)
1. Let _rhs_ be AssignmentExpressionEcmaify(_assign_.expression).
1. Let _op_ be an empty Parse Node.
1. If _assign_.operator is "*=" then set _op_ to *AssignmentOperator* : **<tt>&#42;=</tt>**.
1. Else, if _assign_.operator is "/=" then set _op_ to *AssignmentOperator* : **<tt>/=</tt>**.
1. Else, if _assign_.operator is "%=" then set _op_ to *AssignmentOperator* : **<tt>%=</tt>**.
1. Else, if _assign_.operator is "+=" then set _op_ to *AssignmentOperator* : **<tt>+=</tt>**.
1. Else, if _assign_.operator is "-=" then set _op_ to *AssignmentOperator* : **<tt>-=</tt>**.
1. Else, if _assign_.operator is "<<=" then set _op_ to *AssignmentOperator* : **<tt><<=</tt>**.
1. Else, if _assign_.operator is ">>=" then set _op_ to *AssignmentOperator* : **<tt>>>=</tt>**.
1. Else, if _assign_.operator is ">>>=" then set _op_ to *AssignmentOperator* : **<tt>>>>=</tt>**.
1. Else, if _assign_.operator is "&=" then set _op_ to *AssignmentOperator* : **<tt>&=</tt>**.
1. Else, if _assign_.operator is "^=" then set _op_ to *AssignmentOperator* : **<tt>^=</tt>**.
1. Else, if _assign_.operator is "|=" then set _op_ to *AssignmentOperator* : **<tt>|=</tt>**.
1. Else, set _op_ to *AssignmentOperator* : **<tt>&#42;&#42;=</tt>**.
1. Return *AssignmentExpression* : _lhs_ _op_ _rhs_.

Ecmaify(_getelem_ : ComputedMemberExpression)

1. Let _expr_ be ExpressionEcmaify(_getelem_.expression).
1. If _getelem_._object is a Super, then:
    1. Let _superProp_ be *SuperProperty* : **<tt>super</tt>** **<tt>[</tt>** _expr_ **<tt>]</tt>**.
    1. Return *MemberExpression* : _superProp_.
1. Else:
    1. Let _base_ be MemberExpressionEcmaify(_getelem_._object).
    1. Return *MemberExpression* : _base_ **<tt>[</tt>** _expr_ **<tt>]</tt>**.

Ecmaify(_hook_ : ConditionalExpression)

1. Let _test_ be LogicalORExpressionEcmaify(_hook_.test).
1. Let _then_ be AssignmentExpressionEcmaify(_hook_.consequent).
1. Let _else_ be AssignmentExpressionEcmaify(_hook_.alternate).
1. Return *ConditionalExpression* : _test_ **<tt>?</tt>** _then_ **<tt>:</tt>** _else_.

Ecmaify(_cls_ : ClassExpression)

TODO

Ecmaify(_func_ : FunctionExpression)

1. If _func_.isAsync is *true*, then:
    1. Let _body_ be *AsyncFunctionBody* : LazyFunctionBodyEcmaify(_func_.body).
    1. Return *AsyncFunctionExpression* : **<tt>async</tt>** **<tt>function</tt>** _func_.name **<tt>(</tt>** Ecmaify(params) **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.
1. Else, if _func_.isGenerator is *true*, then:
    1. Let _body_ be *GeneratorBody* : LazyFunctionBodyEcmaify(_func_.body).
    1. Return *GeneratorExpression* : **<tt>function</tt>** __<tt>*</tt>__ _func_.name **<tt>(</tt>** Ecmaify(params) **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.
1. Else:
    1. Let _body_ be LazyFunctionBodyEcmaify(_func_.body).
    1. Return *FunctionExpression* : **<tt>function</tt>** _func_.name **<tt>(</tt>** Ecmaify(_params_) **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.

Ecmaify(_ident_ : IdentifierExpression)

1. If _ident_.name is "yield", then return *IdentifierReference* : **<tt>yield</tt>**.
1. If _ident_.name is "await", then return *IdentifierReference* : **<tt>await</tt>**.
1. Else:
    1. Let _name_ be *Identifier* : _ident_.name.
    1. Return *IdentifierReference* : _name_.

Ecmaify(_new_ : NewExpression)

1. NOTE: New expressions are canonicalized to the arguments-taking verison.
1. Let _callee_ be MemberExpressionEcmaify(_new_.callee).
1. Let _args_ be ArgumentesEcmaify(_new_.arguments).
1. Return *MemberExpression*: **<tt>new</tt>** _callee_ _args_.

Ecmaify(_newTarget_ : NewTargetExpression)

1. Let _n_ be *NewTarget* : **<tt>new</tt>** **<tt>.</tt>** **<tt>target</tt>**.
1. Return *MetaProperty* : _n_.

Ecmaify(_litObj_ : ObjectExpression)

1. If _litObj_.properties has length 0, then return *ObjectLiteral* : **<tt>{</tt>** **<tt>}</tt>**.
1. Else:
    1. Let _props_ be PropertyDefinitionListEcmaify(_litObj_.properties).
    1. Return *ObjectLiteral* : **<tt>{</tt>** _props_ **<tt>}</tt>**.

Ecmaify(_unary_ : UnaryExpression)

1. Let _operand_ be UnaryExpressionEcmaify(_unary_.operand).
1. If _unary_.operator is "delete", then return *UnaryExpression* : **<tt>delete</tt>** _operand_.
1. If _unary_.operator is "void", then return *UnaryExpression* : **<tt>void</tt>** _operand_.
1. If _unary_.operator is "typeof", then return *UnaryExpression* : **<tt>typeof</tt>** _operand_.
1. If _unary_.operator is "+", then return *UnaryExpression* : **<tt>+</tt>** _operand_.
1. If _unary_.operator is "-", then return *UnaryExpression* : **<tt>-</tt>** _operand_.
1. If _unary_.operator is "~", then return *UnaryExpression* : **<tt>~</tt>** _operand_.
1. Else, return *UnaryExpression* : **<tt>!</tt>** _operand_.

Ecmaify(_getprop_ : StaticMemberExpression)

1. If _getprop_._object is a Super, then:
    1. Let _superProp_ be *SuperProperty* : **<tt>super</tt>** **<tt>.</tt>** _getprop_.property.
    1. Return *MemberExpression* : _superProp_.
1. Else:
    1. Let _base_ be MemberExpressionEcmaify(_getprop_._object).
    1. Return *MemberExpression* : _base_ **<tt>.</tt>** _getprop_.property.

Ecmaify(_litTemplate_ : TemplateExpression)

TODO

Ecmaify(_this_ : ThisExpression)

1. Return *PrimaryExpression* : **<tt>this</tt>**.

Ecmaify(_upd_ : UpdateExpression)

1. If _upd_.isPrefix is **true**, then:
    1. Let _operand_ be UnaryExpressionEcmaify(_upd_.operand).
    1. If _upd_.operator is "++", then return *UpdateExpression* : **<tt>++</tt>** _operand_.
    1. Else, return *UpdateExpression* : **<tt>--</tt>** _operand_.
1. Else:
    1. Let _operand_ be LeftHandSideExpressionEcmaify(_upd_.operand).
    1. If _upd_.operator is "++", then return *UpdateExpression* : _operand_ **<tt>++</tt>**.
    1. Else, return *UpdateExpression* : _operand_ **<tt>--</tt>**.

Ecmaify(_yield_ : YieldExpression)

1. If _yield_.expression is null, then:
    1. Return *YieldExpression* : **<tt>yield</tt>**.
1. Else:
    1. Let _expr_ be AssignmentExpressionEcmaify(_yield_.expression).
    1. Return *YieldExpression* : **<tt>yield</tt>** _expr_.

Ecmaify(_yieldStar_ : YieldStarExpression)

1. Let _expr_ be AssignmentExpressionEcmaify(_yieldStar_.expression).
1. Return *YieldExpression* : **<tt>yield</tt>** **<tt>&ast;</tt>** _expr_.

Ecmaify(_await_ : AwaitExpression)

1. Let _expr_ be UnaryExpressionEcmaify(_await_.expression).
1. Return *AwaitExpression* : **<tt>await</tt>** _expr_.

// Ecmaify : FormalParameters -> Parse Node

Ecmaify(_params_ : FormalParameters)

1. If _params_.items has length 0, then:
    1. If _params_.rest is null, then return *FormalParameters* :.
    1. Else:
        1. Let _rest_ be *FunctionRestParameter* : Ecmaify(_params_.rest).
        1. Return *FormalParameters* : _rest_.
1. Else:
    1. Let _list_ be FormalParameterListEcmaify(_params_.items).
    1. If _params_.rest is null, then return *FormalParameters* : _list_.
    1. Else:
        1. Let _rest_ be *FunctionRestParameter* : Ecmaify(_params_.rest).
        1. Return *FormalParameters* : _list_ **<tt>,</tt>** _rest_.

// Ecmaify : MethodDefinition -> Parse Node

Ecmaify(_method_ : Method)

1. Let _name_ be *PropertyName* : Ecmaify(_method_.name).
1. If _method_.isAsync is *true*, then:
    1. Let _params_ be *UniqueFormalParameters* : Ecmaify(params).
    1. Let _body_ be *AsyncFunctionBody* : LazyFunctionBodyEcmaify(_method_.body).
    1. Let _m_ be *AsyncMethod* : **<tt>async</tt>** _name_ **<tt>(</tt>** _params_ **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.
    1. Return *MethodDefinition* : _m_.
1. Else, if _method_.isGenerator is *true*, then:
    1. Let _params_ be *UniqueFormalParameters* : Ecmaify(params).
    1. Let _body_ be *GeneratorBody* : LazyFunctionBodyEcmaify(_method_.body).
    1. Let _m_ *GeneratorMethod* : __<tt>*</tt>__ _name_ **<tt>(</tt>** _params_ **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.
    1. Return *MethodDefinition* : _m_.
1. Else:
    1. Let _params_ be Ecmaify(params).
    1. Let _body_ be LazyFunctionBodyEcmaify(_method_.body).
    1. Return *MethodDefinition* : _name_ **<tt>(</tt>** _params_ **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.

Ecmaify(_getter_ : Getter)

1. Let _name_ be *PropertyName* : Ecmaify(_getter_.name).
1. Let _body_ be LazyFunctionBodyEcmaify(_getter_.body).
1. Return *MethodDefinition* : **<tt>get</tt>** _name_ **<tt>(</tt>** **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.

Ecmaify(_setter_ : Setter)

1. Let _name_ be *PropertyName* : Ecmaify(_setter_.name).
1. Let _param_ be *PropertySetParameterList* : Ecmaify(_setter_.param).
1. Let _body_ be LazyFunctionBodyEcmaify(_setter_.body).
1. Return *MethodDefinition* : **<tt>set</tt>** _name_ **<tt>(</tt>** _param_ **<tt>)</tt>** **<tt>{</tt>** _body_ **<tt>}</tt>**.

// Ecmaify : ObjectProperty -> Parse Node

Ecmaify(_prop_ : DataProperty)

1. Let _name_ be *PropertyName* : Ecmaify(_prop_.name).
1. Let _expr_ be AssignmentExpressionEcmaify(_prop_.expression).
1. Return *PropertyDefinition* : _name_ **<tt>:</tt>** _expr_.

Ecmaify(_prop_ : ShorthandProperty)

1. Let _ident_ be Ecmaify(_prop_.name).
1. Return *PropertyDefinition* : _ident_.

// Ecmaify : PropertyName -> Parse Node

Ecmaify(_name_ : ComputedPropertyName)

1. Let _expr_ be AssignmentExpressionEcmaify(_name_.expression).
1. Return *ComputedPropertyName* : **<tt>[</tt>** _expr_ **<tt>]</tt>**.

Ecmaify(_name_ : StaticPropertyName)

1. NOTE: Literal property names are canonicalized to double quoted string literals.
1. Let _v_ be the value of _name_.value escaped for double quotes.
1. Let _l_ be *StringLiteral* : **<tt>&#34;</tt>** _v_ **<tt>&#34;</tt>**.
1. Return *LiteralPropertyName* : _l_.

// Ecmaify : Parameter -> Parse Node

Ecmaify(_param_ : Parameter)

1. Return *FormalParameter* : Ecmaify(_param_).

// Ecmaify : VariableDeclarationKind -> Parse Node

Ecmaify(_k_ : VariableDeclarationKind kind)

1. Assert _k_ is "let" or "const".
1. If _k_ is "let", then return *LetOrConst* : **<tt>let</tt>**.
1. Else, return *LetOrConst* : **<tt>const</tt>**.

// Ecmaify : ForInOfBinding -> Parse Node

Ecmaify(_b_ : ForInOfBinding)

1. Let _binding_ be *ForBinding* : Ecmaify(_b_.binding).
1. If _b_.kind is "var", then:
    1. Return the partial Parse Node **<tt>var</tt>** _binding_.
1. Else, return *ForDeclaration* : Ecmaify(_b_.kind) _binding_.

// Ecmaify : CatchClause -> Parse Node

Ecmaify(_catchClause_ : CatchClause)

1. Let _binding_ be Ecmaify(_catchClause_.binding).
1. Let _body_ be Ecmaify(_catchClause_.body).
1. Return *Catch* : **<tt>catch</tt>** **<tt>(</tt>** _binding_ **<tt>)</tt>** _body_.
