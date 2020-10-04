# 3. Object Type Not Found

All possible objects known to EnergyPlus are defined in the IDD file.
This rule is trigged when there is no object specified in the IDD file that
matches the given input.

Example of *incorrect* code:

```
TotallyUnknownObjectType,
    No,
    Yes;
```

