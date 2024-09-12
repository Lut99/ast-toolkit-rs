# AST Toolkit - Diagnostics
Amai, this one's really a tough nut to crack, innit?

OK so let's look at this logically. What is the point?

API:
- We have `Diagnostic`s, which represents a single error message from the compiler. I.e., every diagnostic has one "failure" it represents.
    - As such: there is always a toplevel message and span.
    - Then every of such messages can have arbitrary additional annotations attached to it:
        - A highlighted area, with an optional message (with optional colour / type); or
        - A suggestion, which changes the source text instead of merely highlighting it.

Then let's make two levels of Diagnostics:
- There's the `Diagnostic` builder, which is a factory; and then
- There's the `DiagnosticMessage` itself which is like a frozen version of it.
