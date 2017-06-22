# servant-persistent-realworld

[![Build Status](https://travis-ci.org/jkachmar/servant-persistent-realworld.png)](https://travis-ci.org/githubuser/servant-persistent-realworld)

## Warnings

### On Overengineering

The RealWorld project aims to provide examples that aren't overcomplicated or
overengineered so that people unfamiliar with a framework can see what an
idiomatic MVP written with it looks like.

Contrary to that statement, this project feels pretty overengineered at first
glance. I mainly started this as a learning exercise to better familiarize 
myself with [servant-auth], [persistent], web API programming, and larger (more
practical) Haskell applications.

### On Security

*ACHTUNG!*: per the RealWorld specification, this application uses JWTs for user
authentication. I haven't had my implementations audited by anyone, and I'm not 
especially knowledgable about web security, so caveat emptor!

## Getting Started

### Prerequisites

Before building this application, ensure the following dependencies are 
installed on your system:

- [stack](https://docs.haskellstack.org/en/stable/README/)
- [PostgreSQL](https://www.postgresql.org)*

*This application was developed on PostgreSQL 9.6.3, but should be compatible
with any recent version of PostgreSQL supporting the `uuid-ossp` extension.

### TL;DR

To get started as quickly as possible:

    git clone git@github.com/jkachmar/servant-persistent-realworld
    cd servant-persistent-realworld
    stack setup

    cd resources
    make init

    cd ../../
    ssh-keygen -t rsa -b 4096 -N "" -f ./resources/keys/realWorldDemo.key
    stack build
    stack exec realworld

The rest of this README will try to explain in-depth what is happening during
the steps above.

#### Get the Project

Clone this repository and navigate to the directory

    git clone git@github.com/jkachmar/servant-persistent-realworld
    cd servant-persistent-realworld

Install the Glasgow Haskell Compiler (GHC) and related tooling

    stack setup

#### Initialize, Migrate, and Seed the Database

A `Makefile` and some SQL scripts have been included to help set up and teardown
an example database and seed it with test data. To keep things relatively simple,
the scripts are set up assuming a local environment with the standard Postgres 
defaults.

Navigate to the `resources` directory and initialize the database:

    make init

This will create a `srw_db` database, a `srw_user` user to manage it, and install 
the `uuid-ossp` extension.

## Resources
A lot of this code has been copied, adapted, or at the very least least inspired
by the following examples/libraries:

- [Matt Parsons]' [servant-persistent]
- [Chris Allen] and [Alexey Zabelin]'s [cards-with-comrades]
- Example code from the [servant-auth] repository
- And possibly many more that have slipped my mind!

[servant-auth]: https://github.com/plow-technologies/servant-auth
[persistent]: https://github.com/yesodweb/persistent
[Matt Parsons]: https://github.com/parsonsmatt
[servant-persistent]: https://github.com/parsonsmatt/servant-persistent
[Chris Allen]: https://github.com/bitemyapp
[Alexey Zabelin]: https://github.com/alexeyzab
[cards-with-comrades]: https://github.com/alexeyzab/cards-with-comrades
