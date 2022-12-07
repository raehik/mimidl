# Further work & investigations
## Better UI
As of 2022-12-06, there's hardly any UI at all. Just some functions that start
things off conveniently, to be used in GHCi. In approximate priority order:

  * CLI
  * Built-in decryption, combining (FFmpeg, perhaps something more lightweight)
  * Built-in transcoding (FFmpeg)
  * GUI
  * Integrate with browser

## Download original/better quality tracks
The API indicates that FLACs are also available for some albums. Trying to
download them gave me permission errors. I have little doubt that only select
users (e.g. the uploader) can access these. Still, I can't help but wonder if
there's a way for us to download them.

## Code refactoring
  * Improve configuration: do something inspired by those various blog posts
    about combining config files and envvars and CLI options.
    * Primarily, I want to be able to fill in a bunch of environmental data
      using a public file, and authentication data using another (private) file.
  * I'm not getting the composition I want with sum errors and logs. Where do I
    decide on precise error/log order? Shouldn't I separate Mimicle API errors
    from program errors anyway, which I'm now no longer doing?
