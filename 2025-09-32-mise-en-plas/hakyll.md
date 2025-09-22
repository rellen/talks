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
        percent: 1

---

# Summary

<!-- column_layout: [3, 2] -->
<!-- column: 0 -->

Hakyll is a static site generator (SSG) library for Haskell.

It's an Embedded Domain Specific Language (EDSL) for transforming inputs into web markup and assets.

<!-- column: 1 -->

![image:width:40%](logo.png)

<!-- reset_layout -->

-----

# Summary

Hakyll uses an applicative/monadic style that is both clean and potentially daunting to work with for Haskell novices*.

<!-- pause -->
Remove the mystery of the "Module Zoo":
- reading the tutorials
- chatting with LLMs and searching Hoogle to understand the typeclass instances used in the `hakyll-init` starter code
- Hole-Driven Development 

Then it's very much like many other SSGs.

-----

## Disclaimer

What is a Haskell novice?

<!-- pause -->

- not fluent in monads other than the basics like `List` and `Maybe`
- not fluent in 
  - `newtype SomeMonad a = SomeMonad {run/un :: F(..., a, ...)}` or 
  - `data SomeMonad a = SomeMonad (F(..., a, ...))`

<!-- pause -->

Such a novice *could* still have developed programs in Haskell that were used at Australia's largest brewery. 

-----

# Agenda

- The Hakyll EDSL for static websites
- Delights and hardships for the novice Haskeller
- How it works under the hood
- Interactive demo
- Q&A

_____

# The Hakyll EDSL for static websites

- configuration through Haskell code
- primarily concerned with transforming source files to output files
<!-- pause -->
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
-- replace posts/ with blog/ in path 
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

## Templates

```html
<article>
  <section class="header">
    Posted on $date$ $if(author)$ by $author$ $endif$
  </section>
  <section>$body$</section>
</article>
```

the `$date$`, etc. is replaced by that field from the `postCtx`

-----

# Delights and hardships for the novice Haskeller

- Hakyll is well-documented
- the DSL and tooling is great!
- the DSL and workflow is cryptic to get started with

-----

## Delighters

<!-- column_layout: [3, 1] -->
<!-- column: 0 -->
- as we learnt from Joe Armstrong in 1986:
> "declarative programming languages have several advantages over traditional languages"

<!-- column: 1 -->

![image:width:99%](joe.png)

<!-- reset_layout -->
<!-- pause -->
- separation of templates, content, and logic
  - allows `cabal run site watch` to rebuild on changes to templates and content
<!-- pause -->
- super declarative and pipeline-y at first glance
<!-- pause -->
```haskell
match "posts/*" $ do
  route $ setExtension "html"
  compile $
    pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
```

-----

## Delighters
  
- `hakyll-init` is a great starting point - almost no customisation needed to get a site going
<!-- pause -->
- great documentation and examples
  - tutorial series
  - Brent Yorgey's "A guide to the Hakyll module zoo"
  - use Clay as a CSS "preprocessor"

-----

## Things that a novice might struggle with

- it's Haskell, not just a configuration file
  - yes I know that can be the same thing!
<!-- pause -->
- monads!
- debugging?
- cryptic error messages :(
- any custom behaviour means writing Haskell using a "zoo" of types

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

```haskell
match "hole/*" $ do
  route idRoute
  compile $ pandocCompiler >>= _hole
```      

-----

## Hole-Driven Development (HDD) to the rescue

```haskell
 • Found hole: _hole :: Item String -> Compiler (Item a0)
      Where: ‘a0’ is an ambiguous type variable
      Or perhaps ‘_hole’ is mis-spelled, or not in scope
    • In the second argument of ‘(>>=)’, namely ‘_hole’
      In the second argument of ‘($)’, namely ‘pandocCompiler >>= _hole’
      In a stmt of a 'do' block: compile $ pandocCompiler >>= _hole
    • Relevant bindings include
        siteCtx :: Context String (bound at site.hs:18:7)
        copyrightYearsCtx :: Context String (bound at site.hs:17:3)
        main :: IO () (bound at site.hs:15:1)
      Valid hole fits include
        relativizeUrls :: Item String -> Compiler (Item String)
          (imported from ‘Hakyll’ at site.hs:5:1-13
           (and originally defined in ‘Hakyll.Web.Html.RelativizeUrls’))
        renderPandoc :: Item String -> Compiler (Item String)
          (imported from ‘Hakyll’ at site.hs:5:1-13
           (and originally defined in ‘Hakyll.Web.Pandoc’))
        ...
```

_____

# How it works under the hood
_____


## The Rules Monad

```haskell
-- Rules is the main configuration monad
main :: IO ()
main = hakyll $ do  
    match "images/*" $ do -- This is the Rules monad
        route idRoute
        compile copyFileCompiler
```

- Core of Hakyll's configuration system
- Defines what should be processed and how
- Describes the build pipeline
- Not actually executed until the Rules are compiled

---

## The Rules Monad: Key Functions

```haskell
-- Define what files to process
match :: Pattern -> Rules () -> Rules ()

-- Create files that don't exist in the source
create :: [Identifier] -> Rules () -> Rules ()

-- Define how files should be routed
route :: Routes -> Rules ()

-- Define how content should be processed
compile :: Compiler (Item a) -> Rules ()
```

---

## The Compiler Monad

```haskell
-- A typical compiler chain
compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls
```

- Responsible for actual content transformation
- Handles dependencies between items
- Manages content loading and saving
- Uses bind (`>>=`) for processing chains

---

## The Compiler Monad: Key Functions

```haskell
-- Convert markup to HTML
pandocCompiler :: Compiler (Item String)

-- Apply a template to content
loadAndApplyTemplate :: Identifier
                     -> Context a
                     -> Item a
                     -> Compiler (Item String)

-- Fix URLs to be relative
relativizeUrls :: Item String -> Compiler (Item String)

-- Get metadata from items
getMetadata :: Identifier -> Compiler Metadata
```

---

## The Item Functor

```haskell
-- An Item contains content and an identifier
data Item a = Item
    { itemIdentifier :: Identifier
    , itemBody       :: a
    }

-- Items are functors we can map over
fmap :: (a -> b) -> Item a -> Item b
```

- Container for content being processed
- Essential for composition in the Compiler monad

---

## The Item Functor: Usage

```haskell
-- Transform the content directly
-- withItemBody :: (String -> String) -> Item String -> Item String
compile $ getResourceBody
    >>= withItemBody (replace "foo" "bar")

-- Create items from scratch
makeItem :: a -> Compiler (Item a)
```

---

## The Context Monoid

```haskell
-- A Context provides fields for templates
type Context a = String -> [String] -> Item a -> Compiler String
```

- Central to template rendering
- Maps field names to content values

---

## The Context Monoid: Common Fields

```haskell
-- Default context provides basic fields
defaultContext :: Context a

-- Add a date field from metadata
dateField :: String -> String -> Context a

-- Add a title field
titleField :: String -> Context a

-- Access any metadata field
metadataField :: Context a

-- Combine contexts with (<>)
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> titleField "title"
       <> defaultContext
```

---

## The Context Monoid: Custom Fields

```haskell
-- Create a custom field
customField :: String -> (Item a -> Compiler String) -> Context a

-- Example: Create a word count field
wordCountField :: String -> Context String
wordCountField key = field key $ \item -> do
    let wordCount = length . words . itemBody $ item
    return $ show wordCount
    
-- Usage
postCtx = wordCountField "wordCount"
       <> defaultContext
```

---

## The Identifier Type

```haskell
-- Unique identifier for content items
newtype Identifier = Identifier
    { unIdentifier :: FilePath
    } deriving (...)

-- Creating identifiers
fromFilePath :: FilePath -> Identifier
fromCapture :: Pattern -> String -> Identifier
```

- Represents a resource in the site
- Used for targeting and dependencies
- Dependency tracking

---

## The Pattern Type

```haskell
-- Used for matching files
data Pattern = ...

-- Create patterns
fromGlob :: String -> Pattern        -- "posts/*.md"
fromRegex :: String -> Pattern       -- "^posts/.*$"
fromList :: [Identifier] -> Pattern  -- Specific files

-- Combine patterns
(.&&.) :: Pattern -> Pattern -> Pattern  -- AND
(.||.) :: Pattern -> Pattern -> Pattern  -- OR
complement :: Pattern -> Pattern         -- NOT
```

- Used to select which files to process
- Supports glob patterns, regex, and explicit lists
- Can be combined with logical operators
- Essential for organizing content processing

---

## The Snapshot System

```haskell
-- Store intermediate versions
saveSnapshot :: String -> Item a -> Compiler (Item a)

-- Load snapshots from other items
loadSnapshot :: Identifier -> String -> Compiler (Item a)

-- Common snapshot names
-- "content" - Raw content before templates
-- "rendered" - Final HTML after all processing
```

- Allows saving intermediate processing states
- Enables referencing content across items
- Key for building archive pages, RSS feeds, etc.
- Critical for cross-referencing content


-----

# Interactive Demo


