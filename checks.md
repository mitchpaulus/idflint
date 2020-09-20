# Checks for idf-lint

## 1. Minimum number of fields

If an object specifies a minimum number of fields and the object
specified does not have that amount.

## 2. Field value does match available keys

Certain fields are given a choice, such as "Yes" or "No". This error is
flagged when the input does not match any of the available choices.

## 3. Object type not found

This is flagged when there is no object in the idd file specified that
matches the given input.

## 4. Too many fields provided

The idd defines all possible fields. EnergyPlus may not crash with extra
fields given depending on the object, but this is still bad practice.

## 5. Numeric field not entered as numeric

Fields can be either Alphanumeric or numeric. This error is flagged
when the field is expected to be a number and it is not.


