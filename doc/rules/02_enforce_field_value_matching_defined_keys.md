# 2. Enforce Field Value Matching Defined Keys

Certain fields are given a choice, such as "Yes" or "No". This error is
flagged when the input does not match any of the available choices. See
`\key` field level comments in the IDD.

Example of *incorrect* code:

```
PerformancePrecisionTradeoffs,
    BadChoice;
```

Here, `PerformancePrecisionTradeoffs` should be `Yes` or `No`.
