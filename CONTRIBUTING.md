# Contributing to Moshell

🎉 Thanks for your interest in Moshell!

## Way to Contribute

Moshell is a young project, there are many ways to take part in this adventure:

### Try out

Moshell targets being a scripting language for the shell, by incorporating a more rigorous syntax that is intended to be less error-prone and more readable.
If you are interested in this idea, please try it out and give us feedback in the discussion part.
We encourage you to report any issue or inconsistency you may encounter by creating an issue.

### Write content

Documentation helps people understand the language and its features.
Beyond the language itself, code documentation is also a good way to learn about the project.

### Fix interesting issues

Try your hand at fixing issues that are interesting to you. Some are labeled with [`good first issue`](https://github.com/moshell-lang/moshell/labels/good%20first%20issue), this can be a good start to get familiar with the codebase.

Feel free to ask questions in the discussion part or in the issue itself.

## Pull Requests

Pull requests are the primary way to contribute code to Moshell. Pull requests should be made against the latest `master` branch, and created from a new branch in your fork of the repository.
Feel free to rebase your branch on the latest `master` branch before opening a pull request. This will keep your changes up to date.
In your fork of the repository, you can do the following to update your branch:

```sh
git remote add upstream https://github.com/moshell-lang/moshell.git # Add the upstream repository if not already done
git switch master && git pull upstream master # Pull the latest changes
git switch <your-branch> && git rebase master # Rebase your branch on the latest changes
```

We encourage you to keep your pull requests as small as possible. Concise changes have a better chance of being accepted quickly.

When you make a pull request, our CI will build your changes and run them through all the tests and style checks. All of these tests should pass before your pull request can be accepted.
It is configured to run on Linux, with the latest stable version of Rust, GCC 10 and GCC 12.

## Code Guidelines

Moshell is built using the latest stable version of the Rust compiler. Nightly and unstable features should be avoided.

The code in Moshell strives to be idiomatic. The Rust compiler should emit no warnings when building the project. Additionally, all code should be formatted using [rustfmt](https://github.com/rust-lang/rustfmt) and linted using [clippy](https://github.com/rust-lang/rust-clippy).

You can run these tools if you have installed the [Rust toolchain](https://www.rust-lang.org):

```sh
cargo fmt # Format the code
cargo clippy --tests # Lint the code and tests
```

The virtual machine is written in C++20, compiled with Clang and GCC. Compiler specific features should be avoided.

It is built using CMake. The code should be formatted using [clang-format v16](https://clang.llvm.org/docs/ClangFormat.html).

`clang-format` may be run with the following command:

```sh
cd vm && make format # Format the code
```

## Testing

Changes are expected to be tested. Automated tests are written using the [Rust testing utilities](https://doc.rust-lang.org/book/ch11-00-testing.html) and [lang_tester](https://github.com/softdevteam/lang_tester/) for VM tests.

Add a new test either in the `tests` directory of the crate or in the same file as the code it tests.
If a change breaks other tests, please address them.

Run tests with:

```sh
cargo test
```

## Commit Message Guidelines

Describe your changes in imperative mood and in the present tense. The first line should be a short summary of the commit, ideally less than 72 characters.

Use keywords to close/address issues when applicable (`close #138`).
