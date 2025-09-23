---
title: "Mise en plas et Cinq S"
sub_title: "Ash has \"a place for everything and everything in its place\""
options:
  end_slide_shorthand: true
theme:
  name: light
  override:
    default:
      margin:
        percent: 8
      colors:
        background: "edd1b0"
    code:
      padding:
        horizontal: 1
      minimum_margin: 
        percent: 1
      theme_name: base16-ocean.dark

---

# A developer's journey into ambitious, multi-faceted software

<!-- incremental_lists: true -->
<!-- list_item_newlines: 2 -->
<!-- pause -->

## Elixir: year 1-2

- Building software for the edge
- "Collectors" moving data from A to B
- `GenServer`, `Supervisor`, `Application`
- no HTML, JSON, or SQL in sight


## Elixir: year 3+

- persistence
- third-party integrations
- human interaction 

-----

## Code organsation

Web <-> ? <-> Repository 

API <-> ? <-> Third-party APIs

-----

## Code organsation

<!-- incremental_lists: true -->

<!-- pause -->
### Asking myself 
- Where should I put this code ...?
- How can I introduce this cross-cutting concern ...? 

<!-- new_lines: 1 -->

### What I'm really asking
- ... in a way that keeps the codebase organised ...?
- ... in a way that is consistent ...?
- ... so it works with all the other cross-cutting concerns ...?


<!-- new_lines: 2 -->

### What I'm really asking
- ... that I won't "forget" to do each time under a deadline?
- ... and is easy for others to find, understand, and maintain?


-----

<!-- alignment: center -->
<!-- new_lines: 4 -->
Ash stumbled upon me one day in Surrey Hills, Sydney...

-----

![image:width:100%](images/ash-iceberg.png)

<!-- alignment: right -->
_https://alembic.com.au/blog/essence-of-ash-framework_

-----

## Realisation

Ash provides _an_ answer to my questions...



_____

# A place for everything and everything in its place

![image:width:80%](images/shadow-board.jpg)
<!-- alignment: center -->
_[1]_      

_____

# A place for everything and everything in its place

<!-- pause -->
Two workspace/workplace organisation terms come to mind...

<!-- list_item_newlines: 2 -->
<!-- incremental_lists: true -->
- Mise en place
- 5S

_____


## Mise en place - [mi zɑ̃ ˈplas]

_putting in place_ / _gathering_


<!-- column_layout: [1, 1] -->
<!-- column: 0 -->

![image:width:100%](images/mise-en-place-hot-station.jpg)
<!-- alignment: right -->
_[2]_


<!-- column: 1 -->

![image:width:100%](images/mise-en-place-bowls.jpg)
<!-- alignment: right -->
_[3]_

<!-- reset_layout -->

_https://en.wikipedia.org/wiki/Mise_en_place_

-----


## 5S - workplace organisation method

Originating from Japanese JIT manufacturing

<!-- column_layout: [4, 9] -->

<!-- pause -->
<!-- column: 1 -->
![image:width:90%](images/resource-corner-5s.jpg)
<!-- alignment: center -->
_[4]_      


<!-- column: 0 -->
<!-- alignment: left -->
<!-- pause -->

- seiri (整理)
- seiton (整頓)
- seisō (清掃)
- seiketsu (清潔)
- shitsuke (躾)

<!-- new_lines: 1 -->

<!-- pause -->

- sort
- set in order
- shine
- standardise
- sustain


<!-- reset_layout -->
<!-- alignment: center -->
https://en.wikipedia.org/wiki/5S_(methodology)
_____

_____

## Mise en place - edge Collectors

<!-- new_lines: 2 -->
We have our ingredients in place from OTP and the Elixir standard library:
<!-- list_item_newlines: 2 -->
<!-- pause -->
- `GenServer`
- `Supervisor`
- `Application`
      
_____

## Mise en place - Ash ingredients

<!-- new_lines: 2 -->
The ingredients we have gathered are Ash's declarative DSL
<!-- pause -->
<!-- new_lines: 2 -->
Resources, actions, cross-cutting concerns, and the pieces they're made of
<!-- pause -->
<!-- new_lines: 2 -->
Let's see some of the ingredients we can work with

_____


### Resource

The nouns of our system
<!-- pause -->
<!-- new_lines: 2 -->
```elixir
defmodule MyApp.Blog.Post do
  use Ash.Resource, data_layer: AshPostgres.DataLayer

  attributes do
    uuid_primary_key :id

    attribute :title, :string, allow_nil?: false
    attribute :content, :string, allow_nil?: false
    attribute :published_at, :utc_datetime, allow_nil?: true

    timestamps()
  end
end
```

_____

### Actions

The verbs of our system
<!-- pause -->
```elixir {1-20|2-8|11-15|16-20}
defmodule MyApp.Blog.Post do
  actions do
    default: [:read]

    create :create_draft do
      accept [:title, :content] 
    end
  end
end

Post
|> Ash.Changeset.for_create(:create_draft,
    %{title: "Title", content: "Some content"},
    actor: current_user)
|> Ash.create!()

Post
|> Ash.Query.for_read(:read, actor: current_user)
|> Ash.Query.filter(expr(published_at < ago(30, :day)))
|> Ash.create!()
```


_____


### Actions

```elixir {1-17|9-11|13-15}
defmodule MyApp.Blog.Post do
  actions do
    default: [:read]

    create :create_draft do
      accept [:title, :content] 
    end

    read :drafts do
      filter expr(is_nil(:published_at))
    end

    update :publish do
      change set_attribute(:published_at, expr(now()))
    end
  end
end
```

_____

### Actions

```elixir {9-11|1-13}
defmodule MyApp.Blog.Post do
  actions do
    default: [:read]

    create :create_draft do
      accept [:title, :content] 
    end

    update :publish do
      change set_attribute(:published_at, expr(now()))
    end
  end
end
```


_____

### Actions - validate input/pre-conditions

```elixir {10-11|1-15}
defmodule MyApp.Blog.Post do
  actions do
    default: [:read]

    create :create_draft do
      accept [:title, :content] 
    end

    update :publish do
      validate absent(:published_at)

      change set_attribute(:published_at, expr(now()))
    end
  end
end
```
_____

### But `Post`s need authors...

```elixir
defmodule MyApp.Accounts.User do
  use Ash.Resource, data_layer: AshPostgres.DataLayer

  attributes do
    uuid_primary_key :id

    attribute :name, :string, allow_nil?: false
  end
end
```


_____

### Relationships 

```elixir 
defmodule MyApp.Blog.Post do
  actions do
    create :create_draft do
      accept [:title, :content] 
    end
  end
end
```

_____

### Relationships - relational data

```elixir {9-11|6|14|15-17|19-22}
defmodule MyApp.Blog.Post do
  actions do
    create :create_draft do
      accept [:title, :content] 

      change relate_actor(:author)
    end
  end
  relationships do
    belongs_to :author, MyApp.Accounts.User, allow_nil?: false 
  end
end

> some_post = Ash.get!("post_id", actor: :admin)  
   %MyApp.Blog.Post{
     author: %Ash.NotLoaded{}
   }

> Ash.load(some_post, [:author], actor: :admin)  
   %MyApp.Blog.Post{
     author: %{MyApp.Accounts.User{name: "Brooklyn Bloggs"}}
   }
```


_____

### Relationships - relational data

```elixir 
defmodule MyApp.Accounts.User do
  relationships do
    has_many :posts, MyApp.Post, allow_nil?: true 
  end
end
```
_____

### Fancy relationships 

```elixir {5-11}
defmodule MyApp.Accounts.User do
  relationships do
    has_many :posts, MyApp.Blog.Post, allow_nil?: true 

    has_one :first_post, MyApp.Blog.Post do
      allow_nil?: true

      filter (not(is_nil(published_at)))

      sort published_at: :asc
    end
  end
end
```

_____

### Authorization 

```elixir 
defmodule MyApp.Blog.Post do
  actions do
    update :change_title do
      accept [:title] 
    end
  end
end
```

_____

### Authorization - policies

```elixir {8-17|9-11|13-16}
defmodule MyApp.Blog.Post do
  actions do
    update :change_title do
      accept [:title] 
    end
  end

  policies do
    policy action_type(:update) do
      authorize_if relates_to_actor_via([:user])
    end

    policy action_type(:read) do
      authorize_if relates_to_actor_via([:user])
      authorize_if expr(not(is_nil(published_at)))
    end
  end
end
```
_____

### Derived data - calculations

```elixir
defmodule MyApp.Blog.Post do
  attributes do
    attribute :published_at, :utc_datetime, allow_nil?: true
  end
end
```

_____

### Derived data - calculations

```elixir {1-11|6-10|13-16|16|18}
defmodule MyApp.Blog.Post do
  attributes do
    attribute :published_at, :utc_datetime, allow_nil?: true
  end

  calculations do
    calculate :status, :atom,
       expr(if is_nil(published_at),
         do: :draft, else: :published)
  end
end

some_post
|> Ash.Changeset.for_update(:publish, actor: current_user)
|> Ash.Changeset.update!()
|> Ash.load!([:status], actor: current_user)

  %MyApp.Blog.Post{status: :published}
```

----

## Mise en place - Ash ingredients

<!-- column_layout: [1, 3, 4, 1] -->
<!-- column: 1 -->
<!-- new_lines: 2 -->
<!-- pause -->
We've seen:
<!-- pause -->
- resources
- actions
- validations
- relationships
- authorisation
- calculations

<!-- pause -->
Not to mention:

<!-- incremental_lists: true -->
- aggregates
- preparations
- identities
- multi-tenancy
- notifiers
- pubsub
- ...

<!-- column: 2 -->

<!-- jump_to_middle -->
All of these are defined declaratively, with consistent options, e.g. `load`, and fit together well

----

## Mise en place - making a meal

----

### Code interfaces

```elixir 
defmodule MyApp.Blog.Post do
  actions do
    create :create_draft do
      accept [:title, :content] 

      change relate_actor(:author)
    end
  end
```

----

### Code interfaces

```elixir {1-20|10-12|15}
defmodule MyApp.Blog.Post do
  actions do
    create :create do
      accept [:title, :content] 

      change relate_actor(:author)
    end
  end

  code_interface do
    define :create_draft, :create, args: [:title, :content]
  end
end

Post.create_draft("Title", "Some content", actor: current_user)
```
_____

### Domains - an essential ingredient ignored until now

<!-- pause -->
Domains: bounded contexts with the dial turned up to 11
<!-- pause -->

```elixir {1-20|4-10|8|13}
defmodule MyApp.Blog do
  use Ash.Domain, ...

  resources do
    resource MyApp.Blog.Post do
      define :create_draft, :create
      define :list_posts, :read
      define :find_post, :read, get_by: :id
    end
  end
end

MyApp.Blog.find_post!("some_post_id", ...)
```

<!-- pause -->
<!-- new_lines: 1 -->
Domains convinced me that Ash is an answer to my code organisation concerns
_____


## Mise en place - the next level of Ash ingredients

<!-- pause -->
Ash lets us to customise our own ingredients

_____

### Derived data - function calculation

```elixir {7-10}
defmodule MyApp.Blog.Post do
  attributes do
    attribute :title, :string, allow_nil?: false
  end

  calculations do
    calculate :slug, :string,
      fn records, _ctx ->
        Enum.map(records, &slugify(&1.title))
      end
  end
end
```

_____

### Actions - change modules

```elixir {1-20, 11-14}
defmodule MyApp.Blog.Post do
  actions do
    default: [:read]

    create :create_draft do
      accept [:title, :content] 
    end

    update :publish do
      change set_attribute(:published_at, expr(now()))
      change Post.Changes.ScheduleEmailBlast
    end
  end
end
```

_____

### Actions - change modules

```elixir {1-20, 5}
defmodule Post.Changes.ScheduleEmailBlast do
  use Ash.Resource.Change

  def change(changeset, _opts, context) do
    Ash.Changeset.after_action(fn _changeset, record ->
      result = 
        EmailMessage
        |> Ash.Changeset.for_create(:schedule, ...record, ...)
        |> Ash.create()

      case result do
        {:ok, _ } -> {:ok, record}
        {:error, _} -> {:error, ???}
      end
    end)
  end
end
```

____

### Actions - change modules

```elixir
defmodule Post.Changes.ScheduleEmailBlast do
  use Ash.Resource.Change

  def change(changeset, _opts, context) do
    Ash.Changeset.after_action(fn _changeset, record ->
      result = 
        EmailMessage
        |> Ash.Changeset.for_create(:schedule, ...record, ...)
        |> Ash.create()

      case result do
        {:ok, _ } -> {:ok, record}
        {:error, _} -> {:error, ???}
      end
    end)
  end
end
```
<!-- pause -->
This is exactly how builtins, like `set_attribute` are implemented under the hood

____

### Behaviour modules

<!-- new_lines: 1 -->
<!-- pause -->
We just saw a change module
<!-- new_lines: 1 -->
<!-- pause -->
Module implementations can be made for other features:
<!-- new_lines: 1 -->
<!-- incremental_lists: true -->
<!-- list_item_newlines: 2 -->
- calculations
- validations
- preparations

<!-- new_lines: 1 -->
<!-- pause -->
An opportunity for code reuse

____

## How Ash and its practices support a 5S process

-----

### Sort

<!-- pause -->
Remove unnecessary items

<!-- pause -->
Ash:
<!-- pause -->
- declarative DSL
- (sensible) defaults
  
____

### Set in order

<!-- pause -->
Put necessary items in the optimal place

<!-- pause -->
Ash:
<!-- incremental_lists: true -->
- (almost) everything about a resource is in, or hangs off, the module
- a logical order to the DSL
- formatter, code completion
- a naming convention (CRUD, actions, changes, hooks etc)

____

### Shine

<!-- pause -->
Cleaning and inspecting the workspace on a regular basis
<!-- pause -->
Ash:
<!-- incremental_lists: true -->
- we can see when inline, imperative code is building up
- we can see more easily where there might be opportunities for reuse
- formatter
- lean into defaults to reduce noise

____

### Standardize

<!-- pause -->
Establish procedures and schedules to ensure the repetiton of the first three S

<!-- pause -->
Ash:
<!-- pause -->
- DSL
- patterns / "the Ash way to do x" - e.g. action lifecycle hooks, wrapping APIs
____

### Sustain

<!-- pause -->
Ensure that 5S is followed

<!-- pause -->
Ash: 
<!-- pause -->
- not so much the framework itself, but... 
<!-- pause -->
- training
- doc improvements
- taking feedback and improving
- encouraging/supporting community extensions
- ...

_____

# Image attributions

- [1] Shadow board
  - https://upload.wikimedia.org/wikipedia/commons/9/98/Papan_Bayangan_%28Shadow_Board%29.jpg
  - Encik Tekateki, CC BY-SA 4.0 https://creativecommons.org/licenses/by-sa/4.0> via Wikimedia Commons
<!-- new_lines: 1 -->
- [2] Hot station
  - https://www.flickr.com/photos/haynes/500435491,
  - Charles Haynes, CC BY-SA 2.0, https://commons.wikimedia.org/w/index.php?curid=35488828
<!-- new_lines: 1 -->
- [3] Ingredients
  - https://unsplash.com/photos/a-bunch-of-bowls-filled-with-different-types-of-food-3GArfxm0p9M
  - Shaun Tilburg

_____

# Image attributions

- [4] Resource corner
  - https://commons.wikimedia.org/wiki/File:Resource_corner_5S_Safety_close-up_Scanfil_Sieradz.jpg
  - Adrian Grycuk, CC BY-SA 3.0 PL https://creativecommons.org/licenses/by-sa/3.0/pl/deed.en, via Wikimedia Commons
