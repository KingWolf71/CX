# LJ2 Language Quick Reference

## Overview
LJ2 is a simple programming language with C-like syntax, supporting integers, floats, strings, functions, and basic control flow.

## Type System

### Type Suffixes
Variables can have explicit type annotations using suffixes:
- `.f` - Float type
- `.s` - String type
- `.d` - Double type
- No suffix - Integer (default) or inferred from value

### Variable Declaration
```lj2
// Explicit type declaration
x.f = 3.14;           // Float
name.s = "Hello";     // String
count = 42;           // Integer

// Type inferred from literal
y = 2.71828;          // Float (has decimal point)
msg = "World";        // String (quoted)
```

### Important Type Rules
1. **First assignment determines type** - Use type suffix on first assignment
2. **Reassignment uses no suffix** - `x.f = 10.5; x = 20.3;` (correct)
3. **Function return values** - Don't use type suffix when receiving: `r = myFunc();` (not `r.f = myFunc();`)

## Functions

### Function Definition
```lj2
func functionName(param1, param2, param3)
{
    // Function body
    return value;
}

// With typed parameters
func calculate(a.f, b.f, message.s)
{
    result.f = a + b;
    return result;
}
```

### Function Rules
1. **Parameters should have type annotations** for clarity
2. **Local variables need type suffixes** on first assignment
3. **Functions are defined before or after use** (no forward declaration needed in newer versions)

## Built-in Functions

```lj2
print(value1, value2, ...);    // Print values
putc(char);                    // Print single character
random(max, min);              // Random number
concat(str1, val1, ...);       // Concatenate values to string
typeof(variable);              // Get type name
```

## Built-in Constants
```lj2
#pi   or   #PI                 // Pi constant (3.14159...)
```

## Operators

### Arithmetic
`+` `-` `*` `/` `%` (add, subtract, multiply, divide, modulo)

### Comparison
`<` `<=` `>` `>=` `==` `!=`

### Logical
`&&` (and), `||` (or), `!` (not)

### Assignment
`=` (assignment only, no compound operators like +=)

## Control Flow

### If-Else
```lj2
if (condition) {
    // statements
}
else {
    // statements
}
```

### While Loop
```lj2
while (condition) {
    // statements
}
```

## Arrays
```lj2
dim arrayName(size);           // Declare array
dim floatArray.d(10);          // Declare float array

arrayName(0) = 42;             // Access element (0-based)
value = arrayName(3);
```

## Preprocessor

### Pragmas
```lj2
#pragma console on
#pragma appname "My Program"
#pragma consolesize "800x600"
#pragma decimals 4
```

### Macros
```lj2
#define CONSTANT_VALUE 100
#define SQUARE(x) ((x) * (x))
#define MAX(a,b) if((a)>(b)){ res=a; } else { res=b; }
```

## String Concatenation
```lj2
str = "Hello" + " " + "World";   // String concatenation
str = "Value: " + 42.5;          // Automatic conversion
```

## Comments
```lj2
// Single line comment

/*
   Multi-line comment
*/
```

## Common Compilation Errors

### "Expecting SemiColon but found ASSIGN"
**Cause:** Missing type suffix on variable declaration
```lj2
// Wrong:
a = 17.88;
r = test(a);

// Correct:
a.f = 17.88;  // Add .f suffix for float
r = test(a);
```

### "Expecting ASSIGN but found CALL"
**Cause:** Type suffix on function return assignment
```lj2
// Wrong:
r.f = myFunction();

// Correct:
r = myFunction();  // No type suffix when receiving return value
```

### Function parameter errors
**Cause:** Missing type annotations on function parameters
```lj2
// Less clear (may cause issues):
func test(a, b, c) { }

// Better - use type annotations:
func test(a.f, b.f, c.s) { }
```

## Best Practices

1. **Always use type suffixes on first variable declaration**
2. **Add type annotations to function parameters** for clarity
3. **Don't use type suffixes on reassignments** or return value assignments
4. **Use meaningful variable names**
5. **Keep functions focused** on single tasks
6. **Add comments** to explain complex logic

## Example Program
```lj2
// Calculate area of circle
#define PI 3.14159

func circleArea.f(radius.f)
{
    area.f = PI * radius * radius;
    return area;
}

r.f = 5.0;
result.f = circleArea(r);
print("Circle with radius ", r, " has area ", result);
```

## File Extensions
- `.lj` - LJ2 source files

## Compilation
The LJ2 compiler processes source files through stages:
1. Lexical Analysis (tokenization)
2. Syntax Analysis (AST generation)
3. Code Generation
4. Virtual Machine execution
