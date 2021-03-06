## Overview

RabbitMQ projects use pull requests to discuss, collaborate on and accept code contributions.
Pull requests is the primary place of discussing code changes.

## How to Contribute

The process is fairly standard:

 * Fork the repository or repositories you plan on contributing to
 * Create a branch with a descriptive name in the relevant repositories
 * Make your changes, run tests, commit with a [descriptive message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html), push to your fork
 * Submit pull requests with an explanation what has been changed and **why**
 * Be patient. We will get to your pull request eventually

If what you are going to work on is a substantial change, please first ask the core team
of their opinion on [RabbitMQ mailing list](https://groups.google.com/forum/#!forum/rabbitmq-users).


## Code of Conduct

See [CODE_OF_CONDUCT.md](./CODE_OF_CONDUCT.md).


## Running Tests

To run a "fast suite" (a subset of tests):

    make ct-fast

To run a "slow suite" (a subset of tests that take much longer to run):

    make ct-slow

To run a particular suite:

    make ct-$suite_name

for example, to run the `backing_queue` suite:

    make ct-backing_queue

Finally,

    make tests

will run all suites.

## Where to Ask Questions

If something isn't clear, feel free to ask on our [mailing list](https://groups.google.com/forum/#!forum/rabbitmq-users).
