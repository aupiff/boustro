module Server where

import Signal as S
import Http

serverUrl = "http://192.168.1.212:8000/"

fileName : Signal String
fileName = S.constant "main_street.txt"

-- TODO do a drop updates thing so this doesn't happen twice
textContent : Signal String
textContent = let req = S.map (\x -> Http.get (serverUrl ++ "texts/"  ++ x)) fileName
                  response = Http.send <| req
                  getContent : Http.Response String -> String
                  getContent response = case response of
                      Http.Success str -> defaultText
                      Http.Waiting     -> defaultText
                      Http.Failure _ _ -> defaultText
              in S.map getContent response

defaultText ="""
Against Interpretation

Susan Sontag

"Content is a glimpse of something, an encounter like a flash. It's very tiny - very tiny, content." - Willem De Kooning, in an interview

"It is only shallow people who do not judge by appearances. The mystery of the world is the visible, not the invisible." - Oscar Wilde, in a letter

The earliest experience of art must have been that it was incantatory, magical; art was an instrument of ritual. (Cf. the paintings in the caves at Lascaux, Altamira, Niaux, La Pasiega, etc.) The earliest theory of art, that of the Greek philosophers, proposed that art was mimesis, imitation of reality.

It is at this point that the peculiar question of the value of art arose. For the mimetic theory, by its very terms, challenges art to justify itself.

Plato, who proposed the theory, seems to have done so in order to rule that the value of art is dubious. Since he considered ordinary material things as themselves mimetic objects, imitations of transcendent forms or structures, even the best painting of a bed would be only an "imitation of an imitation." For Plato, art is neither particularly useful (the painting of a bed is no good to sleep on), nor, in the strict sense, true. And Aristotle's arguments in defense of art do not really challenge Plato's view that all art is an elaborate trompe l'oeil, and therefore a lie. But he does dispute Plato's idea that art is useless. Lie or no, art has a certain value according to Aristotle because it is a form of therapy. Art is useful, after all, Aristotle counters, medicinally useful in that it arouses and purges dangerous emotions.

In Plato and Aristotle, the mimetic theory of art goes hand in hand with the assumption that art is always figurative. But advocates of the mimetic theory need not close their eyes to decorative and abstract art. The fallacy that art is necessarily a "realism" can be modified or scrapped without ever moving outside the problems delimited by the mimetic theory.

The fact is, all Western consciousness of and reflection upon art have remained within the confines staked out by the Greek theory of art as mimesis or representation. It is through this theory that art as such - above and beyond given works of art - becomes problematic, in need of defense. And it is the defense of art which gives birth to the odd vision by which something we have learned to call "form" is separated off from something we have learned to call "content," and to the well-intentioned move which makes content essential and form accessory.

Even in modern times, when most artists and critics have discarded the theory of art as representation of an outer reality in favor of the theory of art as subjective expression, the main feature of the mimetic theory persists. Whether we conceive of the work of art on the model of a picture (art as a picture of reality) or on the model of a statement (art as the statement of the artist), content still comes first. The content may have changed. It may now be less figurative, less lucidly realistic. But it is still assumed that a work of art is its content. Or, as it's usually put today, that a work of art by definition says something. ("What X is saying is ...," "What X is trying to say is ... ," "What X said is ..." etc., etc.)

2

None of us can ever retrieve that innocence before all theory when art knew no need to justify itself, when one did not ask of a work of art what it said because one knew (or thought one knew) what it did. From now to the end of consciousness, we are stuck with the task of defending art. We can only quarrel with one or another means of defense. Indeed, we have an obligation to overthrow any means of defending and justifying art which becomes particularly obtuse or onerous or insensitive to contemporary needs and practice.

This is the case, today, with the very idea of content itself. Whatever it may have been in the past, the idea of content is today mainly a hindrance, a nuisance, a subtle or not so subtle philistinism.

Though the actual developments in many arts may seem to be leading us away from the idea that a work of art is primarily its content, the idea still exerts an extraordinary hegemony. I want to suggest that this is because the idea is now perpetuated in the guise of a certain way of encountering works of art thoroughly ingrained among most people who take any of the arts seriously. What the overemphasis on the idea of content entails is the perennial, never consummated project of interpretation. And, conversely, it is the habit of approaching works of art in order to interpret them that sustains the fancy that there really is such a thing as the content of a work of art.

3

Of course, I don't mean interpretation in the broadest sense, the sense in which Nietzsche (rightly) says, "There are no facts, only interpretations." By interpretation, I mean here a conscious act of the mind which illustrates a certain code, certain "rules" of interpretation.

Directed to art, interpretation means plucking a set of elements (the X, the Y, the Z, and so forth) from the whole work. The task of interpretation is virtually one of translation. The interpreter says, Look, don't you see that X is really - or, really means - A? That Y is really B? That Z is really C?

What situation could prompt this curious project for transforming a text? History gives us the materials for an answer. Interpretation first appears in the culture of late classical antiquity, when the power and credibility of myth had been broken by the "realistic" view of the world introduced by scientific enlightenment. Once the question that haunts post-mythic consciousness - that of the seemliness of religious symbols - had been asked, the ancient texts were, in their pristine form, no longer acceptable.  Then interpretation was summoned, to reconcile the ancient texts to "modern" demands. Thus, the Stoics, to accord with their view that the gods had to be moral, allegorized away the rude features of Zeus and his boisterous clan in Homer's epics.  What Homer really designated by the adultery of Zeus with Leto, they explained, was the union between power and wisdom. In the same vein, Philo of Alexandria interpreted the literal historical narratives of the Hebrew Bible as spiritual paradigms. The story of the exodus from Egypt, the wandering in the desert for forty years, and the entry into the promised land, said Philo, was really an allegory of the individual soul's emancipation, tribulations, and final deliverance. Interpretation thus presupposes a discrepancy between the clear meaning of the text and the demands of (later) readers.  It seeks to resolve that discrepancy. The situation is that for some reason a text has become unacceptable; yet it cannot be discarded. Interpretation is a radical strategy for conserving an old text, which is thought too precious to repudiate, by revamping it. The interpreter, without actually erasing or rewriting the text, is altering it. But he can't admit to doing this. He claims to be only making it intelligible, by disclosing its true meaning. However far the interpreters alter the text (another notorious example is the Rabbinic and Christian "spiritual" interpretations of the clearly erotic Song of Songs), they must claim to be reading off a sense that is already there.

Interpretation in our own time, however, is even more complex. For the contemporary zeal for the project of interpretation is often prompted not by piety toward the troublesome text (which may conceal an aggression), but by an open aggressiveness, an overt contempt for appearances. The old style of interpretation was insistent, but respectful; it erected another meaning on top of the literal one. The modern style of interpretation excavates, and as it excavates, destroys; it digs "behind" the text, to find a sub-text which is the true one. The most celebrated and influential modern doctrines, those of Marx and Freud, actually amount to elaborate systems of hermeneutics, aggressive and impious theories of interpretation. All observable phenomena are bracketed, in Freud's phrase, as manifest content. This manifest content must be probed and pushed aside to find the true meaning - the latent content - beneath. For Marx, social events like revolutions and wars; for Freud, the events of individual lives (like neurotic symptoms and slips of the tongue) as well as texts (like a dream or a work of art) - all are treated as occasions for interpretation. According to Marx and Freud, these events only seem to be intelligible. Actually, they have no meaning without interpretation. To understand is to interpret. And to interpret is to restate the phenomenon, in effect to find an equivalent for it.

Thus, interpretation is not (as most people assume) an absolute value, a gesture of mind situated in some timeless realm of capabilities. Interpretation must itself be evaluated, within a historical view of human consciousness. In some cultural contexts, interpretation is a liberating act. It is a means of revising, of transvaluing, of escaping the dead past. In other cultural contexts, it is reactionary, impertinent, cowardly, stifling.

4

Today is such a time, when the project of interpretation is largely reactionary, stifling.  Like the fumes of the automobile and of heavy industry which befoul the urban atmosphere, the effusion of interpretations of art today poisons our sensibilities. In a culture whose already classical dilemma is the hypertrophy of the intellect at the expense of energy and sensual capability, interpretation is the revenge of the intellect upon art.

Even more. It is the revenge of the intellect upon the world. To interpret is to impoverish, to deplete the world - in order to set up a shadow world of "meanings." It is to turn the world into this world. ("This world"! As if there were any other.)

The world, our world, is depleted, impoverished enough. Away with all duplicates of it, until we again experience more immediately what we have.

5

In most modern instances, interpretation amounts to the philistine refusal to leave the work of art alone. Real art has the capacity to make us nervous. By reducing the work of art to its content and then interpreting that, one tames the work of art. Interpretation makes art manageable, comformable.

This philistinism of interpretation is more rife in literature than in any other art. For decades now, literary critics have understood it to be their task to translate the elements of the poem or play or novel or story into something else. Sometimes a writer will be so uneasy before the naked power of his art that he will install within the work itself - albeit with a little shyness, a touch of the good taste of irony - the clear and explicit interpretation of it. Thomas Mann is an example of such an overcooperative author. In the case of more stubborn authors, the critic is only too happy to perform the job.

The work of Kafka, for example, has been subjected to a mass ravishment by no less than three armies of interpreters. Those who read Kafka as a social allegory see case studies of the frustrations and insanity of modern bureaucracy and its ultimate issuance in the totalitarian state. Those who read Kafka as a psychoanalytic allegory see desperate revelations of Kafka's fear of his father, his castration anxieties, his sense of his own impotence, his thralldom to his dreams. Those who read Kafka as a religious allegory explain that K. in The Castle is trying to gain access to heaven, that Josepl K.  in The Trial is being judged by the inexorable and mysterious justice of God. . . .  Another oeuvre that has attracted interpreters like leeches is that of Samuel Beckett.  Beckett's delicate dramas of the withdrawn consciousness - pared down to essentials, cut off, often represented as physically immobilized - are read as a statement about modern man's alienation from meaning or from God, or as an allegory of psychopathology.

Proust, Joyce, Faulkner, Rilke, Lawrence, Gide... one could go on citing author after author; the list is endless of those around whom thick encrustations of interpretation have taken hold. But it should be noted that interpretation is not simply the compliment that mediocrity pays to genius. It is, indeed, the modern way of understanding something, and is applied to works of every quality. Thus, in the notes that Elia Kazan published on his production of A Streetcar Named Desire, it becomes clear that, in order to direct the play, Kazan had to discover that Stanley Kowalski represented the sensual and vengeful barbarism that was engulfing our culture, while Blanche Du Bois was Western civilization, poetry, delicate apparel, dim lighting, refined feelings and all, though a little the worse for wear to be sure. Tennessee Williams' forceful psychological melodrama now became intelligible: it was about something, about the decline of Western civilization. Apparently, were it to go on being a play about a handsome brute named Stanley Kowalski and a faded mangy belle named Blanche Du Bois, it would not be manageable.

6

It doesn't matter whether artists intend, or don't intend, for their works to be interpreted. Perhaps Tennessee Williams thinks Streetcar is about what Kazan thinks it to be about. It may be that Cocteau in The Blood of a Poet and in Orpheus wanted the elaborate readings which have been given these films, in terms of Freudian symbolism and social critique. But the merit of these works certainly lies elsewhere than in their "meanings." Indeed, it is precisely to the extent that Williams' plays and Cocteau's films do suggest these portentous meanings that they are defective, false, contrived, lacking in conviction.

From interviews, it appears that Resnais and Robbe-Grillet consciously designed Last Year at Marienbad to accommodate a multiplicity of equally plausible interpretations.  But the temptation to interpret Marienbad should be resisted. What matters in Marienbad is the pure, untranslatable, sensuous immediacy of some of its images, and its rigorous if narrow solutions to certain problems of cinematic form.

Again, Ingmar Bergman may have meant the tank rumbling down the empty night street in The Silence as a phallic symbol. But if he did, it was a foolish thought. ("Never trust the teller, trust the tale," said Lawrence.) Taken as a brute object, as an immediate sensory equivalent for the mysterious abrupt armored happenings going on inside the hotel, that sequence with the tank is the most striking moment in the film.  Those who reach for a Freudian interpretation of the tank are only expressing their lack of response to what is there on the screen.

It is always the case that interpretation of this type indicates a dissatisfaction (conscious or unconscious) with the work, a wish to replace it by something else.

Interpretation, based on the highly dubious theory that a work of art is composed of items of content, violates art. It makes art into an article for use, for arrangement into a mental scheme of categories.

7

Interpretation does not, of course, always prevail. In fact, a great deal of today's art may be understood as motivated by a flight from interpretation. To avoid interpretation, art may become parody. Or it may become abstract. Or it may become ("merely") decorative. Or it may become non-art.

The flight from interpretation seems particularly a feature of modern painting. Abstract painting is the attempt to have, in the ordinary sense, no content; since there is no content, there can be no interpretation. Pop Art works by the opposite means to the same result; using a content so blatant, so "what it is," it, too, ends by being uninterpretable.

A great deal of modern poetry as well, starting from the great experiments of French poetry (including the movement that is misleadingly called Symbolism) to put silence into poems and to reinstate the magic of the word, has escaped from the rough grip of interpretation. The most recent revolution in contemporary taste in poetry - the revolution that has deposed Eliot and elevated Pound - represents a turning away from content in poetry in the old sense, an impatience with what made modern poetry prey to the zeal of interpreters.

I am speaking mainly of the situation in America, of course. Interpretation runs rampant here in those arts with a feeble and negligible avant-garde: fiction and the drama. Most American novelists and playwrights are really either journalists or gentlemen sociologists and psychologists. They are writing the literary equivalent of program music. And so rudimentary, uninspired, and stagnant has been the sense of what might be done with form in fiction and drama that even when the content isn't simply information, news, it is still peculiarly visible, handier, more exposed. To the extent that novels and plays (in America), unlike poetry and painting and music, don't reflect any interesting concern with changes in their form, these arts remain prone to assault by interpretation.

But programmatic avant-gardism - which has meant, mostly, experiments with form at the expense of content - is not the only defense against the infestation of art by interpretations. At least, I hope not. For this would be to commit art to being perpetually on the run. (It also perpetuates the very distinction between form and content which is, ultimately, an illusion.) Ideally, it is possible to elude the interpreters in another way, by making works of art whose surface is so unified and clean, whose momentum is so rapid, whose address is so direct that the work can be . . . just what it is. Is this possible now? It does happen in films, I believe. This is why cinema is the most alive, the most exciting, the most important of all art forms right now. Perhaps the way one tells how alive a particular art form is, is by the latitude it gives for making mistakes in it, and still being good. For example, a few of the films of Bergman - though crammed with lame messages about the modern spirit, thereby inviting interpretations - still triumph over the pretentious intentions of their director. In Winter Light and The Silence, the beauty and visual sophistication of the images subvert before our eyes the callow pseudointellectuality of the story and some of the dialogue. (The most remarkable instance of this sort of discrepancy is the work of D. W. Griffith.) In good films, there is always a directness that entirely frees us from the itch to interpret. Many old Hollywood films, like those of Cukor, Walsh, Hawks, and countless other directors, have this liberating anti-symbolic quality, no less than the best work of the new European directors, like Truffaut's Shoot the Piano Player and Jules and Jim, Godard's Breathless and Vivre Sa Vie, Antonioni's L'Avventura, and Olmi's The Fiancés.

The fact that films have not been overrun by interpreters is in part due simply to the newness of cinema as an art. It also owes to the happy accident that films for such a long time were just movies; in other words, that they were understood to be part of mass, as opposed to high, culture, and were left alone by most people with minds.  Then, too, there is always something other than content in the cinema to grab hold of, for those who want to analyze. For the cinema, unlike the novel, possesses a vocabulary of forms - the explicit, complex, and discussable technology of camera movements, cutting, and composition of the frame that goes into the making of a film.

8

What kind of criticism, of commentary on the arts, is desirable today? For I am not saying that works of art are ineffable, that they cannot be described or paraphrased.  They can be. The question is how. What would criticism look like that would serve the work of art, not usurp its place?

What is needed, first, is more attention to form in art. If excessive stress on content provokes the arrogance of interpretation, more extended and more thorough descriptions of form would silence. What is needed is a vocabulary - a descriptive, rather than prescriptive, vocabulary - for forms [1]. The best criticism, and it is uncommon, is of this sort that dissolves considerations of content into those of form.  On film, drama, and painting respectively, I can think of Erwin Panofsky's essay, "Style and Medium in the Motion Pictures," Northrop Frye's essay "A Conspectus of Dramatic Genres," Pierre Francastel's essay "The Destruction of a Plastic Space." Roland Barthes' book On Racine and his two essays on Robbe-Grillet are examples of formal analysis applied to the work of a single author. (The best essays in Erich Auerbach's Mimesis, like "The Scar of Odysseus," are also of this type.) An example of formal analysis applied simultaneously to genre and author is Walter Benjamin's essay, "The Story Teller: Reflections on the Works of Nicolai Leskov."

Equally valuable would be acts of criticism which would supply a really accurate, sharp, loving description of the appearance of a work of art. This seems even harder to do than formal analysis. Some of Manny Farber's film criticism, Dorothy Van Ghent's essay "The Dickens World: A View from Todgers'," Randall Jarrell's essay on Walt Whitman are among the rare examples of what I mean. These are essays which reveal the sensuous surface of art without mucking about in it.

9

Transparence is the highest, most liberating value in art - and in criticism - today.  Transparence means experiencing the luminousness of the thing in itself, of things being what they are. This is the greatness of, for example, the films of Bresson and Ozu and Renoir's The Rules of the Game.

Once upon a time (say, for Dante), it must have been a revolutionary and creative move to design works of art so that they might be experienced on several levels. Now it is not. It reinforces the principle of redundancy that is the principal affliction of modern life.

Once upon a time (a time when high art was scarce), it must have been a revolutionary and creative move to interpret works of art. Now it is not. What we decidedly do not need now is further to assimilate Art into Thought, or (worse yet) Art into Culture.

Interpretation takes the sensory experience of the work of art for granted, and proceeds from there. This cannot be taken for granted, now. Think of the sheer multiplication of works of art available to every one of us, superadded to the conflicting tastes and odors and sights of the urban environment that bombard our senses. Ours is a culture based on excess, on overproduction; the result is a steady loss of sharpness in our sensory experience. All the conditions of modern life - its material plenitude, its sheer crowdedness - conjoin to dull our sensory faculties. And it is in the light of the condition of our senses, our capacities (rather than those of another age), that the task of the critic must be assessed.

What is important now is to recover our senses. We must learn to see more, to hear more, to feel more.

Our task is not to find the maximum amount of content in a work of art, much less to squeeze more content out of the work than is already there. Our task is to cut backcontent so that we can see the thing at all.

The aim of all commentary on art now should be to make works of art - and, by analogy, our own experience - more, rather than less, real to us. The function of criticism should be to show how it is what it is, even that it is what it is, rather than to show what it means.

10

In place of a hermeneutics we need an erotics of art.

[1964]

1 One of the difficulties is that our idea of form is spatial (the Greek metaphors for form are all derived from notions of space). This is why we have a more ready vocabulary of forms for the spatial than for the temporal arts. The exception among the temporal arts, of course, is the drama; perhaps this is because the drama is a narrative (i.e., temporal) form that extends itself visually and pictorially, upon a stage. . . .  What we don't have yet is a poetics of the novel, any clear notion of the forms of narration. Perhaps film criticism will be the occasion of a breakthrough here, since films are primarily a visual form, yet they are also a subdivision of literature. o deplete the world -- in order to set up a shadow world of "meanings." It is to turn the world into this world. ("This world"! As if there were any other.) The world, our world, is depleted, impoverished enough. Away with all duplicates of it, until we again experience more immediately what we have.
"""
