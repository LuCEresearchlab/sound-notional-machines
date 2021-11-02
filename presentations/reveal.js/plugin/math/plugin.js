/**
 * A plugin which enables rendering of math equations inside
 * of reveal.js slides. Essentially a thin wrapper for MathJax.
 *
 * @author Hakim El Hattab
 */
const Plugin = () => {

    // The reveal.js instance this plugin is attached to
    let deck;

    let defaultOptions = {
        messageStyle: 'none',
        tex2jax: {
            inlineMath: [
                ['$', '$'],
                ['\\(', '\\)']
            ],
            skipTags: ['script', 'noscript', 'style', 'textarea', 'pre']
        },
        skipStartupTypeset: true
    };

    function loadScript(url, callback) {

        let head = document.querySelector('head');
        let script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = url;

        // Wrapper for callback to make sure it only fires once
        let finish = () => {
            if (typeof callback === 'function') {
                callback.call();
                callback = null;
            }
        }

        script.onload = finish;

        // IE
        script.onreadystatechange = () => {
            if (this.readyState === 'loaded') {
                finish();
            }
        }

        // Normal browsers
        head.appendChild(script);

    }

    return {
        id: 'math',

        init: function(reveal) {

            deck = reveal;

            let revealOptions = deck.getConfig().math || {};

            let options = {...defaultOptions, ...revealOptions };
            let mathjax = options.mathjax || 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js';
            let config = options.config || 'TeX-AMS_HTML-full';
            let url = mathjax + '?config=' + config;

            options.tex2jax = {...defaultOptions.tex2jax, ...revealOptions.tex2jax };

            options.mathjax = options.config = null;

            loadScript(url, function() {

                MathJax.Hub.Config(options);

                // Typeset followed by an immediate reveal.js layout since
                // the typesetting process could affect slide height
                MathJax.Hub.Queue(['Typeset', MathJax.Hub, deck.getRevealElement()]);
                MathJax.Hub.Queue(deck.layout);

                //=====================================
                // Fragments in Mathjax equations
                // usage :
                // simple fragment appearing, e.g.: \fragment{1}{x_1}
                // apply style change to present fragment, e.g.: \fragapply{highlight-blue}{x_1}
                // add specific index to trigger style change for fragment: \fragindex{1}{\fragapply{highlight-blue}{x_1}}

                MathJax.Hub.Register.StartupHook("TeX Jax Ready", function() {
                    const TEX = MathJax.InputJax.TeX;

                    TEX.Definitions.Add({
                        macros: {
                            'fragment': 'FRAGMENT_INDEX_attribute', // regular fragments
                            'fragapply': 'FRAGMENT_apply', // style change to fragments
                            'texclass': 'TEX_APPLY_class' // add any class to an element
                        }
                    });

                    TEX.Parse.Augment({
                        FRAGMENT_INDEX_attribute: function(name) {
                            const index = this.GetArgument(name);
                            const arg = this.ParseArg(name);
                            this.Push(arg.With({
                                'class': 'fragment fragment-mjx',
                                attrNames: ['data-fragment-index'],
                                attr: { 'data-fragment-index': index }
                            }));
                        },
                        FRAGMENT_apply: function(name) {
                            const input = this.GetArgument(name)
                            let [apply_kind, index] = [...input.split(" ")];
                            const arg = this.ParseArg(name);
                            if (index) {
                                this.Push(arg.With({
                                    'class': 'fragapply fragment fragment-mjx ' + apply_kind,
                                    attrNames: ['data-fragment-index'],
                                    attr: { 'data-fragment-index': index }
                                }));
                            } else {
                                this.Push(arg.With({
                                    'class': 'fragapply fragment fragment-mjx ' + apply_kind
                                }));
                            }
                        },
                        TEX_APPLY_class: function(name) {
                            const class_kind = this.GetArgument(name);
                            const arg = this.ParseArg(name);
                            this.Push(arg.With({
                                'class': class_kind
                            }));
                        }
                    }); // TEX.parse.argument
                }); // Mathjax.hub.register

                // Reprocess equations in slides when they turn visible
                deck.on('slidechanged', function(event) {
                    MathJax.Hub.Queue(['Typeset', MathJax.Hub, event.currentSlide]);

                    // clean up false fragment created during Mathjax process
                    const slide = event.currentSlide
                    let slideFragments = Array.prototype.slice.call(slide.querySelectorAll('.fragment-mjx'))
                    slideFragments = slideFragments.filter(d => d.nodeName !== "SPAN")
                    for (let i = 0; i < slideFragments.length; i++) {
                        slideFragments[i].classList.remove("fragment");
                    }
                });

            });

        }
    }

};

export default Plugin;