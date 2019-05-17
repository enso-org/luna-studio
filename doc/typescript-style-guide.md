# TypeScript Style Guide
Like many style guides, this TypeScript style guide exists for two primary
reasons. The first is to provide guidelines that result in a consistent code
style across all of the Luna codebases, while the second is to guide people
towards a style that is expressive while still easy to read and understand.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Code Formatting](#code-formatting)
- [Comments](#comments)
  - [Documentation Comments](#documentation-comments)
  - [Source Notes](#source-notes)
  - [TODO Comments](#todo-comments)
  - [Other Comment Usage](#other-comment-usage)
- [Program Design](#program-design)
  - [Inheritance vs. Composition](#inheritance-vs-composition)

<!-- /MarkdownTOC -->

## Code Formatting
Rather than a whole host of complicated formatting rules for TypeScript, we make
use of [prettier](https://prettier.io), an opinionated code formatter, and
[eslint](https://eslint.org). All TypeScript code should be formatted using
prettier and checked using eslint, though the point at which you do this is up
to you. We recommend either on-save in your editor, or using a precommit hook.

Both the prettier and eslint configurations that can be used are located in the
root of this repository, and should be copied to all other repositories
containing TypeScript code if it is not already there.

Please note that in order to enable smooth prettier and eslint experience in the
Visual Studio Code, you should install the `prettier` and `eslint` plugins, add
the following configuration to the user configuration (File -> Preferences ->
Settings), and restart the IDE.

```json
{
    "editor.formatOnSave": true,
    "eslint.provideLintTask": true,
    "eslint.validate": ["javascript", "typescript"],
}
```

## Comments
Comments are a tricky area to get right, as we have found that comments often
expire quickly and, in absence of a way to validate them, remain incorrect for
long periods of time. That is not to say, however, that we eschew comments
entirely. Instead, we make keeping comments up to date an integral part of our
programming practice, while also limiting the types of comments that we allow.

When we write comments, we try to follow one general guideline. A comment should
explain _what_ and _why_, without mentioning _how_. The _how_ should be
self-explanatory from reading the code, and if you find that it is not, that is
a sign that the code in question needs refactoring.

Code should be written in such a way that it guides you over what it does, and
comments should not be used as a crutch for badly-designed code.

### Documentation Comments
One of the primary forms of comment that we allow across the Luna codebases is
the doc comment. These are intended to be consumed by users of the API, and use
the standard Haddock syntax. Doc comments should:

- Provide a short one-line explanation of the object being documented.
- Provide a longer description of the object, including examples where relevant.
- Explain the arguments to a function where relevant.

They should not reference internal implementation details, or be used to explain
choices made in the function's implementation. See [Source Notes](#source-notes)
below for how to indicate that kind of information.

### Source Notes
Source Notes is a mechanism for moving detailed design information about a piece
of code out of the code itself. In doing so, it retains the key information
about the design while not impeding the flow of the code.

Source notes are detailed comments that, like all comments, explain both the
_what_ and the _why_ of the code being described. In very rare cases, it may
include some _how_, but only to refer to why a particular method was chosen to
achieve the goals in question.

A source note comment is broken into two parts:

1. **Referrer:** This is a small comment left at the point where the explanation
   is relevant. It takes the following form: `// Note [Note Name]`, where
   `Note Name` is a unique identifier across the codebase. These names should be
   descriptive, and make sure you search for it before using it, in case it is
   already in use.
2. **Source Note:** This is the comment itself, which is a large block comment
   placed after the first function in which it is referred to in the module. It
   uses the haskell block-comment syntax `/* ... */`, and the first line names
   the note using the same referrer as above: `/* Note [Note Name]`. The name(s)
   in the note are underlined using a string of the `~` (tilde) character.

A source note may contain sections within it where necessary. These are titled
using the following syntax: `== Note [Note Name (Section Name)]`, and can be
referred to from a referrer much as the main source note can be.

Sometimes it is necessary to reference a source note in another file, but this
should never be done in-line. Instead, a piece of code should reference a source
note in the same module that references the other note while providing
additional context.

An example, taken from the GHC codebase, can be seen below.

```typescript
export abstract class Foo implements MyInterface {
  foo() {
    frobnicate() // Note [Something Complicated]

    return "hello" // Note [Something Complicated (Extra)]
  }

/* Note [Something Complicated]
    ... Some explanation about the topic at hand...

== Note [Something Complicated (Extra)]
    ... Some explanation about the sub-topic at hand...
*/
}
```

A source note like this is useful whenever you have design decisions to explain,
but can also be used for:

- **Formulae and Algorithms:** If your code makes use of a mathematical formula,
  or algorithm, it should note where the design element came from, preferably
  with a link.
- **Safety:** Sometimes it is necessary to use an unsafe API in a context where
  it is trivially made safe. You should always use a source note to explain why
  its usage is safe in this context.

### TODO Comments
We follow a simple convention for `TODO` comments in our codebases:

- The line starts with `TODO` or `FIXME`.
- It is then followed by the author's initials `[ARA]`, or for multiple people
  `[ARA, WD]`, in square brackets.
- It is then followed by an explanation of what needs to be done.

For example:

```typescript
// TODO [ARA] This is a bit of a kludge. Instead of X it should to Y, accounting
// for the fact that Z.
```

### Other Comment Usage
There are, of course, a few other situations where commenting is very useful:

- **Commenting Out:** You may comment out code while developing it, but if you
  commit any commented out code, it should be accompanied by an explanation of
  why said code can't just be deleted.
- **Bugs:** You can use comments to indicate bugs in our code, as well as
  third-party bugs. In both cases, the comment should link to the issue tracker
  where the bug has been reported.

## Program Design
In designing front-end code for Luna, we have a few principles that we tend to
try and stick to as we have found that they produce clearer architecture and
cleaner code.

### Inheritance vs. Composition
In almost all circumstances, Luna's TypeScript codebases tend to favour
[composition over inheritance](https://en.wikipedia.org/wiki/Composition_over_inheritance)
as inheritance is often misused.

In these codebases, we do not use inheritance for the purpose of _sharing code_,
and instead only use it to model the sharing of behaviour. As a result, we have
the following rules of thumb

- Always use composition instead of inheritance.
- If you have a use-case that is made significantly clearer through use of
  inheritance, please discuss it with Wojciech (@wdanilo). If it is decided to
  make an exception, these will be noted here in the style guide.
- When working with composition, make use of the `composable-mixins` library
  (currently contained in the `BaseGL` repository) to eliminate boilerplate and
  create clearer code.

#### Composable Mixins and Avoiding Boilerplate
One of the main issues with using composition architect code is that it can
result in the need for a lot of bookkeeping and boilerplate to expose the
desired functionality to the host object. This can often become quite the chore
in languages without first-class support for composition.

As a result, we've created the `composable-mixins` library, currently located in
the `BaseGL` repository as that library is its primary client for now. It allows
for significantly simpler definition and management of composed objects. Please
refer to the library's documentation for a detailed description of its
functionality, but an example is provided below.

```ts
class C1 {
    private _priv_field = "c1_priv_field"
    c1_field1 : string
    c1_func1(){return this._priv_field}
    get c1_priv_field()  {return this._priv_field}
    set c1_priv_field(v) {this._priv_field = v}
    constructor(cfg:any){
        this.c1_field1 = `c${cfg.id}_field1`
    }
}
const c1 = {c1:C1}


class C2 {
    private _priv_field = "c2_priv_field"
    c2_field1 : string
    c2_func1(){return this._priv_field}
    get c2_priv_field()  {return this._priv_field}
    set c2_priv_field(v) {this._priv_field = v}
    constructor(cfg:any){
        this.c2_field1 = `c${cfg.id}_field1`
    }
}
const c2 = {c2:C2}


class C3 extends mixed (c1,c2) {
    private _priv_field = "c3_priv_field"
    c3_field1 : string
    c3_func1(){return this._priv_field}
    get c3_priv_field()  {return this._priv_field}
    set c3_priv_field(v) {this._priv_field = v}
    constructor(cfg:any){
        super(cfg)
        this.c3_field1 = `c${cfg.id}_field1`
    }
}

const tgt = new C4(
    { id: 3
    , c1: {id: 1}
    , c2: {id: 2}
    })

console.log(tgt.c1_field1)     // "c1_field1"
console.log(tgt.c1.c1_field1)  // "c1_field1"
console.log(tgt.c1_priv_field) // "c1_priv_field"
```
