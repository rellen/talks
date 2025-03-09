---
title: "Make Hakyll while the sun shines"
sub_title: "An introduction to making static websites with Hakyll"
author: Robert Ellen
options:
  end_slide_shorthand: true
theme:
  name: light
  override:
    default:
      colors:
        background: "edd1b0"
    code:
      minimum_margin: 
        percent: 5
---

# Agenda

- Introduction
- Conclusion
- Q&A

-----

# Introduction

- a
- b 
- c

_____

# The Hakyll EDSL for bloggers

- configuration through Haskell code
- primarily concerned with transforming source files to output files
- good starting point: `hakyll-init`

```haskell
match "posts/*" $ do
 route $ setExtension "html"
  compile $
    pandocCompiler -- takes anything pandoc can transform!
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
```
-----

## Basic Hakyll `site.hs` syntax: Rules

```haskell
main :: IO ()
main = hakyll $ do

  match "images/*" $ do ...

  match "css/*.css" $ do ...

  match (fromList ["about.rst", "contact.markdown"]) $ do ...

  match "posts/*" $ do ...

  create ["archive.html"] $ do ...
    -- has as dependency on templates/... 

  match "index.html" $ do ...

  match "templates/*" $ ...
```
_____

## Basic Hakyll `site.hs` syntax: Routes

```haskell
-- Set the route for files
route idRoute  -- Keep the same path

-- Change file extension
route $ setExtension "html"

-- Create pretty URLs (blog/post-name/index.html)
route $ customRoute $ (++ "/index.html") . toFilePath

-- Create custom paths
route $ gsubRoute "posts/" (const "blog/")
```

-----

## Basic Hakyll `site.hs` syntax: Compilers

```haskell
-- Process content with Pandoc
compile $ pandocCompiler

-- Add templates
compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx

-- Create special pages
create ["archive.html"] $ do
    route idRoute
    compile $ makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx

```
-----

# Delights and hardships for the novice Haskeller

- some of the DSL and tooling is great!
- some of the DSL and workflow is cryptic
- sometimes the DSL is both great and cryptic!

## Delighters

- As we learnt from Joe Armstrong in 1986, "declarative programming languages have several advantages over traditional languages"

## Things that a novice might struggle with

- it's Haskell, not just a configuration file
  - yes I know that can be the same thing!
- monads!
- debugging?
- cryptic error messages :(
- any custom behaviour means Haskell

```haskell
-- What's a Compiler? What's a Context?
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

-- What's happening with all these operators?
-- and this sure looks imperative
match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= relativizeUrls
```

-----

## Hole-Driven Development (HDD) to the rescue

_____

# How it works
_____

## `Rules` monad 


_____

## `Compiler` monad 

_____

## `Item` functor 
