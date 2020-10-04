## 7. Duplicate Name for Object in Reference List

An identifier for an object needs to be unique within a reference group
list. An example is that two schedules cannot have the same name.

Example of *incorrect* code:

```
Schedule:Constant,
    Schedule1,
    ,
    1;

Schedule:Constant,
    Schedule1,
    ,
    2;
```

