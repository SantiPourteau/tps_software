package com.example.tp4;

import org.springframework.stereotype.Service;
import org.udesa.unoback.model.*;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Service
public class UnoService {

    private final Map<UUID, Match> activeMatches = new ConcurrentHashMap<>();
    private final Dealer dealer;

    public UnoService(Dealer dealer) {
        this.dealer = dealer;
    }

    public UUID createNewMatch(List<String> players) {
        if (players == null || players.size() < 2) {
            throw new RuntimeException("Se requieren al menos 2 jugadores para crear una partida");
        }

        UUID matchId = UUID.randomUUID();
        List<Card> deck = dealer.fullDeck();
        
        Match match = Match.fullMatch(deck, players);
        activeMatches.put(matchId, match);
        
        return matchId;
    }

    public void playCard(UUID matchId, String player, JsonCard jsonCard) {
        Match match = getMatch(matchId);
        Card card = convertJsonCardToCard(jsonCard);
        match.play(player, card);
    }

    public void drawCard(UUID matchId, String player) {
        Match match = getMatch(matchId);
        match.drawCard(player);
    }

    public JsonCard getActiveCard(UUID matchId) {
        Match match = getMatch(matchId);
        Card activeCard = match.activeCard();
        return convertCardToJsonCard(activeCard);
    }

    public List<JsonCard> getPlayerHand(UUID matchId) {
        Match match = getMatch(matchId);
        List<Card> hand = match.playerHand();
        return hand.stream()
                .map(this::convertCardToJsonCard)
                .collect(Collectors.toList());
    }

    private Match getMatch(UUID matchId) {
        Match match = activeMatches.get(matchId);
        if (match == null) {
            throw new RuntimeException("Partida no encontrada: " + matchId);
        }
        return match;
    }



    private Card convertJsonCardToCard(JsonCard jsonCard) {
        switch (jsonCard.getType()) {
            case "NumberCard":
                NumberCard numberCard = new NumberCard(jsonCard.getColor(), jsonCard.getNumber());
                return jsonCard.isShout() ? numberCard.uno() : numberCard;
            case "SkipCard":
                SkipCard skipCard = new SkipCard(jsonCard.getColor());
                return jsonCard.isShout() ? skipCard.uno() : skipCard;
            case "ReverseCard":
                ReverseCard reverseCard = new ReverseCard(jsonCard.getColor());
                return jsonCard.isShout() ? reverseCard.uno() : reverseCard;
            case "Draw2Card":
                Draw2Card draw2Card = new Draw2Card(jsonCard.getColor());
                return jsonCard.isShout() ? draw2Card.uno() : draw2Card;
            case "WildCard":
                WildCard wildCard = new WildCard();
                if (jsonCard.getColor() != null) {
                    wildCard = (WildCard) wildCard.asColor(jsonCard.getColor());
                }
                return jsonCard.isShout() ? wildCard.uno() : wildCard;
            default:
                throw new RuntimeException("Tipo de carta desconocido: " + jsonCard.getType());
        }
    }

    private JsonCard convertCardToJsonCard(Card card) {
        return card.asJson();
    }
} 