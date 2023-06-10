import codewars_test as test 

# Straight-flush (five consecutive ranks of the same suit). Higher rank is better.
# Four-of-a-kind (four cards with the same rank). Tiebreaker is first the rank, then the rank of the remaining card.
# Full house (three cards with the same rank, two with another). Tiebreaker is first the rank of the three cards, then rank of the pair.
# Flush (five cards of the same suit). Higher ranks are better, compared from high to low rank.
# Straight (five consecutive ranks). Higher rank is better.
# Three-of-a-kind (three cards of the same rank). Tiebreaker is first the rank of the three cards, then the highest other rank, then the second highest other rank.
# Two pair (two cards of the same rank, two cards of another rank). Tiebreaker is first the rank of the high pair, then the rank of the low pair and then the rank of the remaining card.
# Pair (two cards of the same rank). Tiebreaker is first the rank of the two cards, then the three other ranks.
# Nothing. Tiebreaker is the rank of the cards from high to low.

def hand(hole_cards, community_cards):
    card_ord = ['A', 'K', 'Q', 'J', '10', '9', '8', '7', '6', '5', '4', '3', '2']
    suits = ["♠", "♦","♣", "♥"]
 
    all_cards = hole_cards + community_cards
    sum_cards = dict(zip(suits+["∑"],[[[] for _ in range(14)] for _ in range(5)]))
    for c in all_cards:
         cv, cs = c[:-1], c[-1]
         cidx = [i for i,v in enumerate(card_ord) if cv == v]
         for i in cidx:
            sum_cards[cs][i] = [cv+cs]
            sum_cards["∑"][i] += [cv+cs]

    def combo_and_ranked(name, used, n=0):
        combo = list(dict.fromkeys([c[:-1] for c in used]))
        ranked = [rc[0][:-1] for rc in sum_cards["∑"] if rc != [] and rc[0] not in used][:n]
        return (name, combo + ranked)
        
    straight_flush = []
    for s in suits:
        for c in sum_cards[s]:
            if c:
                straight_flush.append(c[0])
            if len(straight_flush) < 5 and not c:
                straight_flush = []
    if len(straight_flush) >= 5:
        return combo_and_ranked('straight-flush', straight_flush[:5])

    flush = []
    for s in suits:
        sames = [c[0] for c in sum_cards[s] if len(c) >= 1]
        if len(sames) >= 5: 
            flush = sames[:5]
    if len(flush) == 5:
        return combo_and_ranked('flush', flush)

    straight = []
    for c in sum_cards["∑"]:
        if c:
            straight.append(c[0])
        if len(straight) < 5 and not c:
            straight = []
    if len(straight) >= 5:
        return combo_and_ranked('straight', straight[:5])

    quads = [c for c in sum_cards["∑"] if len(c) == 4]
    if len(quads) > 0:
        return combo_and_ranked('four-of-a-kind', quads[0], n=1)
    
    trips = [c for c in sum_cards["∑"] if len(c) == 3]
    pairs = [c for c in sum_cards["∑"] if len(c) == 2]
    if len(trips) > 0 and len(pairs) > 0:
        return combo_and_ranked('full house', trips[0]+pairs[0])
    # could be two triplets, take the highest
    if len(trips) > 0: 
        return combo_and_ranked('three-of-a-kind', trips[0], n=2)
    if len(pairs) == 2:
        return combo_and_ranked('two pair', pairs[0]+pairs[1], n=1)
    if len(pairs) == 1:
       return combo_and_ranked('pair', pairs[0], n=3)
    
    return combo_and_ranked('nothing', [], n=5)




@test.describe("Texas Hold'em Hands")
def example_test():

    @test.it("nothing")
    def ex1():
        test.assert_equals(
            hand(["K♠", "A♦"], ["J♣", "Q♥", "9♥", "2♥", "3♦"]),
            ("nothing", ["A", "K", "Q", "J", "9"]),
        )
    @test.it("pair")
    def ex2():
        test.assert_equals(
            hand(["K♠", "Q♦"], ["J♣", "Q♥", "9♥", "2♥", "3♦"]),
            ("pair", ["Q", "K", "J", "9"]),
        )
    @test.it("flush")
    def ex3():
        test.assert_equals(
            hand(["A♠", "K♦"], ["J♥", "5♥", "10♥", "Q♥", "3♥"]),
            ("flush", ["Q", "J", "10", "5", "3"]),
        )
    @test.it("pair")
    def ex4():     
          test.assert_equals(
            hand(["K♠", "Q♦"], ["J♣", "Q♥", "9♥", "2♥", "3♦"]),
            ("pair", ["Q", "K", "J", "9"]),
        )
    @test.it("two pair")
    def ex5():
        test.assert_equals(
            hand(["K♠", "J♦"], ["J♣", "K♥", "9♥", "2♥", "3♦"]),
            ("two pair", ["K", "J", "9"]),
        )
    @test.it("three of a kind")
    def ex6():
        test.assert_equals(
            hand(["4♠", "9♦"], ["J♣", "Q♥", "Q♠", "2♥", "Q♦"]),
            ("three-of-a-kind", ["Q", "J", "9"]),
        )
    @test.it("straight")
    def ex7():
        test.assert_equals(
            hand(["Q♠", "2♦"], ["J♣", "10♥", "9♥", "K♥", "K♦"]),
            ("straight", ["K", "Q", "J", "10", "9"]),
        )

    @test.it("full house")
    def ex8():
        test.assert_equals(
            hand(["A♠", "A♦"], ["K♣", "K♥", "A♥", "Q♥", "3♦"]),
            ("full house", ["A", "K"]),
        )
    @test.it("four of a kind")
    def ex9():
        test.assert_equals(
            hand(["2♠", "3♦"], ["2♣", "2♥", "3♠", "3♥", "2♦"]),
            ("four-of-a-kind", ["2", "3"]),
        )
    @test.it("straight flush")
    def ex10():
        test.assert_equals(
            hand(["8♠", "6♠"], ["7♠", "5♠", "9♠", "J♠", "10♠"]),
            ("straight-flush", ["J", "10", "9", "8", "7"]),
        )
    @test.it("two pair")
    def ex11():
        test.assert_equals(
            hand(['A♦', 'A♥'], ['9♣', '9♥', '2♠', 'Q♥', 'J♠']),
            ('two pair', ['A', '9', 'Q']),
        )
  
