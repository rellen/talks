
#let slide(body) = {
  pagebreak(weak: true)
  block(
    width: 100%,
    height: 100%,
    inset: 2em,
    body
  )
}

#set page(
  paper: "presentation-16-9",
  margin: 0em,
  header: none,
  footer: none
)

#set text(size: 20pt)

// Title slide
#slide[
  #block(width: 100%, height: 100%, inset: 2em)[
    #align(center + horizon)[
      #text(size: 2.5em, weight: "bold")[My Presentation]
      #v(1em)
      #text(size: 1.5em)[Author Name]
      #v(1em)
      #text(size: 1.2em)[Date]
    ]
  ]
]

// Content slide with bullet points
#slide[
  #text(size: 1.5em, weight: "bold")[Introduction]
  #v(1em)
  - First point
  - Second point
    - Sub-point A
    - Sub-point B
  - Third point
]

// Two-column slide
#slide[
  #text(size: 1.5em, weight: "bold")[Two Columns]
  #v(1em)
  #grid(
    columns: (1fr, 1fr),
    gutter: 1em,
    [
      *Left Column*
      - Point 1
      - Point 2
    ],
    [
      *Right Column*
      - Point A
      - Point B
    ]
  )
]

// Image slide
#slide[
  #text(size: 1.5em, weight: "bold")[Adding Images]
  #v(1em)
  #align(center)[
    // Replace with your image path
    // #image("path/to/image.png", width: 60%)
    [Image placeholder]
  ]
]

// Code slide
#slide[
  #text(size: 1.5em, weight: "bold")[Code Example]
  #v(1em)
  ```elixir
  def hello_world() do
    IO.puts("Hello, World!")
  end
  ```
]
