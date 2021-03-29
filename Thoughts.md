# Purpose of Abstraction (Notional Machines)

## What is the `alpha` good for?

One end of the spectrum: `alpha` is reversible (1:1 mapping).

* An expression tree in the form of Scratch blocks vs. an expression tree in the form of a node-link diagram (where one can show the values flowing over the links).
* Shneiderman's paper (and the CACM rejection letter) on Nassi-Shneiderman diagrams, which "just draw boxes" around the source code blocks (which according to the rejection letter are useless).

Other end of the spectrum: `alpha` maps to `unit`.

## What is the abstraction `A` good for?

---
This could be written up as an Onward! Essay (or just paper).

I think Richard Gabriel & Co. would love this.
https://www.dreamsongs.com/
---

The `f` functions tell us what the abstraction `A` is good for.

There is a spectrum of `f` functions:

One the simplistic extreme, there is only an `f` function that maps to `unit`. Then the abstraction is "useless". The abstraction is art!

On the most general extreme, the `f` function maps from `A` to `A`. Then the abstraction is generally useful.

In-between these extremes, somewhare in the middle of the spectrum, we can do useful projections with the `f` function.

### Example: Auralization

One example in the middle of the spectrum (probably more towards the simplistic extreme) is "program auralization": mapping a program execution to a sound. At first glance, this is just art. Once you have an auralization of a program execution, what useful things could you possibly do with it? At a second look, this might actually be useful, as some of the following papers point out, e.g., it could be useful for some kinds of debugging:

* CAITLIN: A Musical Program Auralisation Tool to Assist Novice Programmers with Debugging
  https://www.icad.org/websiteV2.0/Conferences/ICAD96/proc96/vickers.htm
  ```
  In the field of auditory display relatively little work has focused on the use of sound to aid program debugging. In this paper, we describe CAITLIN , a pre-processor for Turbo Pascal programs that musically auralises programs with a view to assisting novice programmers with locating errors in their code. A discussion follows of an experiment which showed that programmers could use the musical feedback to visualise and describe program structure. 
  ```
* Program auralization:
  https://dl.acm.org/doi/abs/10.1145/1101530.1101547?download=true
  ```
  In this paper, we reflect upon the investigations into external auditory representations of programs (program auralization) reported by Vickers and Alty at ICAD 2000. First, we place the work in its historical and thematic context and explore the motivation that lay behind it. We then outline the process by which we got to the stage of being able to report empirical results in 2000 and compare the work with that done by other researchers in the field. Finally, we assess the major contribution that this work made to the field of auditory display and look to the future outlining the work still to be done since the paper was first published (we also look at work done by others in this area since 2000).
  ```
* Increasing Fault Detection Effectiveness Using Layered Program Auralization
  http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.92.3748
  ```
  This paper presents a new approach to using music for debugging computer code, layered program auralization. We use layers of musical structure to represent the state and behavior of a computer program while it is running, taking advantage of metaphorical relationships between musical structure and programming constructs. 
  ```
* Interactive Music and Synchronous Reactive Programming
  https://2021.programming-conference.org/details/programming-2021-papers/19/Interactive-Music-and-Synchronous-Reactive-Programming
  ```
  This paper presents Skini, a programming methodology and an execution environment for interactive structured music. With this system, the composer programs his scores in the HipHop.js synchronous reactive language. They are then executed, or played, in live concerts, in interaction with the audience. The system aims at helping composers to find a good balance between the determinism of the compositions and the nondeterminism of the interactions with the public. Each execution of a Skini score yields to a different but aesthetically consistent interpretation.
  ```
  This is more about "live programming", an art form.
