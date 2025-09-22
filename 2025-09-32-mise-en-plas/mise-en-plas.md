---
title: "Mise en plas et Cinq S"
sub_title: "Ash has \"a place for everything and everything in its place\""
options:
  end_slide_shorthand: true
theme:
  name: light
  override:
    default:
      colors:
        background: "edd1b0"
      font_size: 5
    code:
      minimum_margin: 
        percent: 1

---

# Summary

<!-- column_layout: [3, 2] -->
<!-- column: 0 -->



<!-- column: 1 -->


<!-- reset_layout -->

-----

# Summary

<!-- pause -->

-----

## A journey into ambitious, multi-faceted software

<!-- incremental_lists: true -->
<!-- list_item_newlines: 2 -->

### Year 1-2

- Building software for the edge
- "Collectors" moving data from A to B
- `GenServer`, `Supervisor`, `Application`
- no HTTP or SQL in sight


### Year 3+

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
- Where should I put this code ...?
- How can I introduce this cross-cutting concern ...? 

<!-- new_lines: 2 -->

- ... in a way that keeps the codebase organised ...?
- ... in a way that is consistent ...?
- ... so it works with all the other cross-cutting concerns ...?


<!-- new_lines: 2 -->

- ... that I won't forget to do each time under a deadline ...?
- ... and is easy for others to understand and maintain ...?


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


## Mise en place - [mi zɑ̃ ˈplas]


<!-- column_layout: [2, 2] -->
<!-- column: 0 -->

![image:width:100%](images/mise-en-place-hot-station.jpg)
<!-- alignment: right -->
Charles Haynes - [1]


<!-- column: 1 -->

![image:width:100%](images/mise-en-place-bowls.jpg)
<!-- alignment: right -->
Shaun Tilburg - [2]

<!-- reset_layout -->

-----

## Mise en place - edge Collectors

<!-- new_lines: 2 -->
We have our ingredients in place from OTP and the Elixir standard library:
<!-- list_item_newlines: 2 -->
<!-- pause -->
- GenServer
- Supervisor
- Application
      
_____


## 5S

![image:width:100%](images/resource-corner-5s.jpg)
<!-- alignment: right -->
Shaun Tilburg - [2]

_____

```d2 +render +width:40%
my_table: {
  shape: sql_table
  id: int {constraint: primary_key}
  last_updated: timestamp with time zone
}
```

___

# Image attributions

- [1] Hot station
  - https://www.flickr.com/photos/haynes/500435491,
  - Charles Haynes, CC BY-SA 2.0, https://commons.wikimedia.org/w/index.php?curid=35488828
<!-- new_lines: 1 -->
- [2] Ingredients
  - https://unsplash.com/photos/a-bunch-of-bowls-filled-with-different-types-of-food-3GArfxm0p9M
  - Shaun Tilburg
<!-- new_lines: 1 -->
- [3] Shadow board
  - https://upload.wikimedia.org/wikipedia/commons/9/98/Papan_Bayangan_%28Shadow_Board%29.jpg
  - Encik Tekateki, CC BY-SA 4.0 https://creativecommons.org/licenses/by-sa/4.0> via Wikimedia Commons

___


# Image attributions

- [4] Resource corner
  - https://commons.wikimedia.org/wiki/File:Resource_corner_5S_Safety_close-up_Scanfil_Sieradz.jpg
  - Adrian Grycuk, CC BY-SA 3.0 PL https://creativecommons.org/licenses/by-sa/3.0/pl/deed.en, via Wikimedia Commons
<!-- new_lines: 1 -->
