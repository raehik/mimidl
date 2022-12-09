[mimicle]: https://mimicle.com/

[*(日本語版はこちら）*](README.ja.md)

# mimidl
A set of tools for interacting with a [mimicle][mimicle]-compatible service
programmatically.

## Usage
Requires a Haskell compiler. Use [GHCup](https://www.haskell.org/ghcup/).

    stack run --

Please use the built-in help. If you get stuck, append `--help` to your command.

### Using commands that require authentication
All commands requiring user authentication (e.g. `dl-work`) require the
following flags:

  * `--api-base`: the base mimicle service API URL. For the official mimicle
    website, this is `https://mimicle.com/api`.
  * `--firebase-api-key`: the service's Firebase API key, which identifies it to
    Google Firebase's services. This is public, and you can find it in the
    source of mimicle webpages.

Furthermore, when running the command, you will be prompted for a *user refresh
token*. This is a long-life token identifying you as your logged-in user to
mimicle. This is private, and allows the holder to perform actions as you.
(However, it's probably not possible to do things such as make purchases or
change account security information.)

You must obtain this user refresh token yourself. One way is to borrow the token
stored in your browser cache. For more information, ask the author.

## Investigation
See the [docs](docs) folder for investigations on mimicle service
functionality.
