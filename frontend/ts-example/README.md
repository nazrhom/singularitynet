# `singularitynet` Typescript example

This subdirectory contains a new Typescript project demonstrating how to consume the [Javascript SDK](../package.json) that we created. This document outlines the necessary steps to build and bundle the main JS SDK and then build the Typescript project. It also details some important details for your own frontend integration.

## How to build and run

### Build the main SDK package

We will simulate pulling the main SDK package from NPM by using the SDK directory (i.e. `frontend`) as a local NPM dependency.

_`frontend/ts-example/package.json`_:

```json-with-comments
{
  "name": "singularitynet-example",
  // Omitted for brevity
  "dependencies": {
    "singularitynet": "file:../"
  }
}
```

Since we haven't published the package, we need to build it first using the following steps:

1. Enter the root of the repository
2. Enter the Nix development environment and then the `frontend` directory:
   ```
   $ nix develop .#frontend
   $ cd frontend
   ```
3. Build the SDK
   ```
   $ npm run js:build
   ```

This will output the built package into `dist`. It has been made available under the library name of `singularitynet` and can be imported by package consumers using this name (see the [Webpack configuration](../webpack.config.js)).

### Build the Typescript project

We're now ready to consume the (local) SDK package. If you make any changes in the `frontend` directory, you can easily repeat the previous step to re-build it and update it as a dependency for the TS project.

1. Enter the `frontend/ts-example` directory
2. Install the NPM dependencies to the local directory, including the `singularitynet` package
   ```
   $ npm i
   ```
   **Note**: We do **not** do this before building the main SDK package as the NPM dependencies are handled by Nix in that case. For the current TS example, however, I did not create a similar setup so we need to use `npm` directly.
3. Either start the development server or run the build:
   ```
   $ npm run dev
   ```
   And visit `localhost:4008` in your browser, _or_
   ```
   $ npm run build
   ```
