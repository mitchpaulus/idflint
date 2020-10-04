## 6. Field Not Found in Reference List

Many fields reference lists built from other defined objects. For
example, many schedules can be defined, and in other objects, a schedule
is referenced.

Example of *incorrect* code:

```
Schedule:Constant,
    Constant Schedule,
    Type not found,
    1;
```

Here, the second field was expected to be the "Schedule Type Limits
Name", which was not defined.
