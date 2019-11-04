# Telraam
New and hopefully improved application to count laps of the 12urenloop event

## Requirements
- Java 11 (JDK)

This is just my one cent about how to manage you database using DAO's

We got a database singleton that houses the DataAccessProvider. It's only there as a wrapper to change our actual database implementation. You can change this by a 'WhateverIsMySourceDataAccessProvider' that has all the necessary interfaces and their methods implemented and it will just work fine.

This provider can give us a context which houses one database connection. With this we