# 4. Too Many Fields Provided

The IDD file defines all possible fields for an object type. EnergyPlus
may not crash with extra fields, depending on the object, but this
is bad practice.

Example of *incorrect* code:

```
Version,
    9.2,
    Another Field;
```

The `Version` object only has one field defined in the IDD.
