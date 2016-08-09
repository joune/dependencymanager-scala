# ScalaDM

This is a draft scala API for managing OSGi service components and dependencies based on Felix's [dependencymanager](http://felix.apache.org/documentation/subprojects/apache-felix-dependency-manager.html)

It's a scaled down version, in terms of features; just a place for my own "proof of concept", the goal being to make a more typesafe and more fluent DependencyManager API.

# Sample usage

View tests.

# Features

## Typesafe

* Compiler ensures that

    * the service provided by a component is indeed implemented by the component
    * the various callback methods exist and have the correct type

* The API is driven by types, not class instances

## Pluggable default service configuration

For instance dependencies may be injected unless specified otherwise.
