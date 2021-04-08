# Languages for CFG

Emails:

--------

Message: 1
Date: Tue, 6 Apr 2021 16:42:25 -0500
From: Vanessa McHale <vamchale@gmail.com>
To: haskell-cafe@haskell.org
Subject: Re: [Haskell-cafe] Looking for a toy imperative language
        implementation in Haskell for research
Message-ID: <a82d616c-81d8-ae05-a02b-f369629b9d68@gmail.com>
Content-Type: text/plain; charset="utf-8"

I have kempe - the backend is the interesting part I guess
http://hackage.haskell.org/package/kempe

It has blocks, it's effectful, it has no loops (just recursion) though.

- Vanessa McHale

--------

Message: 2
Date: Wed, 7 Apr 2021 03:56:07 +0530
From: Siddharth Bhat <siddu.druid@gmail.com>
To: Vanessa McHale <vamchale@gmail.com>
Cc: haskell-cafe <haskell-cafe@haskell.org>
Subject: Re: [Haskell-cafe] Looking for a toy imperative language
        implementation in Haskell for research
Message-ID:
        <CAPipc=2LqmQeVqz5FixwZ8B-_1qDn55F8DxNhsdeHGYdpFcqOA@mail.gmail.com>
Content-Type: text/plain; charset="utf-8"

I have https://github.com/bollu/tiny-optimising-compiler which implements a
tiny compiler for SSA. It has everything you ask for. It generates MIPS
asm, so it performs register allocation as well.

--------

Hello Igor,

There is an implementation of a subset of Tcl in Haskell. Not sure if this is
what you need, but I tried playing with it some years ago and found it
interesting.

https://hackage.haskell.org/package/hiccup

--------

Jeff Clites <jclites@mac.com>	Tue, Apr 6, 2021 at 3:54 AM
To: Igor Moreno Santos <igormoreno@gmail.com>
Cc: haskell-cafe@haskell.org
I wonder if something from The Programming Languages Zoo would be helpful:

  https://plzoo.andrej.com/

It’s in OCaml but might be a good starting point.

Jeff

--------

Compl Yue <compl.yue@icloud.com>
Tue, Apr 6, 7:10 AM (2 days ago)
to me, haskell-cafe@haskell.org

https://github.com/e-wrks/edh

It's dynamically typed, not sure you want that, and maybe overkill wrt features. Pending 0.3 release, which has a lot changed since 0.1 and 0.2. The latest branch may appear a bit more stable than the 0.3 branch, yet unrelease anyway.

--------

YueCompl
Apr 7, 2021, 12:47 PM (22 hours ago)
to me

The most up-to-date material wrt the language part, from the upcoming 0.3 release is here: https://github.com/complyue/tour/blob/master/lang/grammar.edh

This grammar chapter is considered almost done while other chapters still wip. 

Best read experience with Gitpod or VSCode as described in https://github.com/complyue/tour

--------

Brent Yorgey <byorgey@gmail.com>
Apr 6, 2021, 4:28 AM (2 days ago)
to me, Haskell

I have an implementation of a language that has all the features you mention (ints + booleans, assignment, arithmetic and logic operators, if statements, while and repeat loops, blocks), which I have used in my Programming Languages class.  It also has a typechecker.  It is interpreted --- not sure if that is OK or if you are looking for something with a compiler.  I would be happy to send it to you privately off-list if it sounds like it might fit the bill.

-Brent
