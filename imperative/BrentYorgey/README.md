Brent answered to Igor's Haskell Café post and offered his own implementation.

---
From: Brent Yorgey <byorgey@gmail.com>
Date: Tue, Apr 6, 2021 at 4:28 AM
Subject: Re: [Haskell-cafe] Looking for a toy imperative language implementation in Haskell for research
To: Igor Moreno Santos <igormoreno@gmail.com>
Cc: Haskell Café <haskell-cafe@haskell.org>


I have an implementation of a language that has all the features you mention (ints + booleans, assignment, arithmetic and logic operators, if statements, while and repeat loops, blocks), which I have used in my Programming Languages class.  It also has a typechecker.  It is interpreted --- not sure if that is OK or if you are looking for something with a compiler.  I would be happy to send it to you privately off-list if it sounds like it might fit the bill.

-Brent

On Mon, Apr 5, 2021 at 7:34 PM Igor Moreno Santos <igormoreno@gmail.com> wrote:
Hi,

I'm looking for a toy imperative language implementation in Haskell for research purposes. I imagine something like the language of arithmetic expressions from TAPL ch. 3 augmented with
- while-loop (so the CFG has loops)
- blocks (sequence of statements to put inside loops and conditionals)
- assignment (otherwise we can't show any effects from sequences)

I think there's probably nothing exactly like that so we might end up doing it ourselves but maybe there's already something out there.

Thank you in advance.

Regards,
Igor Moreno
---
