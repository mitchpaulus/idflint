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

The idd file defines all possible fields. EnergyPlus may not crash with
extra fields given depending on the object, but this is still bad
practice.

## 5. Numeric field not entered as numeric

Fields can be either Alphanumeric or numeric. This error is flagged
when the field is expected to be a number and it is not.

## 6. Field not found in reference list

Many fields reference lists built from other defined objects. For
example, many schedules can be defined, and in other objects, a schedule
is referenced.

## 7. Duplicate name for object in reference list

An identifier for an object needs to be unique within a reference group
list. An example is that two schedules cannot have the same name.


## 8. Numeric value out of range

Many numeric fields need to have values that are above or below certain
thresholds.
