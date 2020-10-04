# 8. Enforce Numeric Value in Range

Many numeric fields need to have values that are above or below certain
thresholds.

Example of *incorrect* code:

```
Timestep,
    0;
```

The value for the `Timestep` is required to be greater than or equal to
1.
