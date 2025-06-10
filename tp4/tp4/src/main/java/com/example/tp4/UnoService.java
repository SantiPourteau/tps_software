package com.example.tp4;

import org.springframework.stereotype.Service;
import org.udesa.unoback.model.*;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Service
public class UnoService {

    private final Map<UUID, Match> activeMatches = new ConcurrentHashMap<>();

    public UUID createNewMatch(List<String> players) {
        if (players == null || players.size() < 2) {
            throw new RuntimeException("Se requieren al menos 2 jugadores para crear una partida");
        }

        UUID matchId = UUID.randomUUID();
        List<Card> deck = createStandardDeck();
        
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

    private List<Card> createStandardDeck() {
        List<Card> deck = new ArrayList<>();
        String[] colors = {"Red", "Blue", "Green", "Yellow"};
        
        // Cartas numericas (0-9) - 2 de cada numero por color (excepto 0)
        for (String color : colors) {
            deck.add(new NumberCard(color, 0)); // Solo una carta 0 por color
            for (int number = 1; number <= 9; number++) {
                deck.add(new NumberCard(color, number));
                deck.add(new NumberCard(color, number));
            }
        }
        
        // Cartas especiales - 2 de cada tipo por color
        for (String color : colors) {
            deck.add(new SkipCard(color));
            deck.add(new SkipCard(color));
            deck.add(new ReverseCard(color));
            deck.add(new ReverseCard(color));
            deck.add(new Draw2Card(color));
            deck.add(new Draw2Card(color));
        }
        
        // Cartas comodin - 4 de cada tipo
        for (int i = 0; i < 4; i++) {
            deck.add(new WildCard());
        }
        
        // Mezclar el mazo
        Collections.shuffle(deck);
        return deck;
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