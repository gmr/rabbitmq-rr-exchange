# RabbitMQ Round-Robin Exchange Plugin

[![Build Status](https://travis-ci.org/gmr/rabbitmq-rr-exchange.svg?branch=master)](https://travis-ci.org/gmr/rabbitmq-rr-exchange)

A RabbitMQ Plugin that adds a round-robin exchange type. Messages routed
through a `x-round-robin` exchange will be distributed evenly across all bound
destinations (queues and/or exchanges).

For example, given 12 messages and the queues Q1, Q2, Q3, and Q4:

- Q1 will receive messages #1, #5, and #9
- Q2 will receive messages #2, #6, and #10
- Q3 will receive messages #3, #7, and #11
- Q4 will receive messages #4, #8, and #12

## Supported RabbitMQ Versions

This plugin requires RabbitMQ 3.7.0 or later.

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) and our [development process overview](http://www.rabbitmq.com/github.html).


## License

[Licensed under the BSD 3-Clause License](LICENSE)


## Copyright

(c) AWeber Communications, 2017
