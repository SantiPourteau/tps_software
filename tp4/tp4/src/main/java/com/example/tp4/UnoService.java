package com.example.tp4;

import com.example.tp4.exceptions.InvalidGameActionException;
import com.example.tp4.exceptions.InvalidGameParametersException;
import com.example.tp4.exceptions.MatchNotFoundException;
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
            throw new InvalidGameParametersException("Se requieren al menos 2 jugadores para crear una partida");
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
        
        try {
            match.play(player, card);
        } catch (RuntimeException e) {
            // Convertir excepciones del modelo a excepciones específicas del negocio
            throw new InvalidGameActionException(e.getMessage(), e);
        }
    }

    public void drawCard(UUID matchId, String player) {
        Match match = getMatch(matchId);
        
        try {
            match.drawCard(player);
        } catch (RuntimeException e) {
            // Convertir excepciones del modelo a excepciones específicas del negocio
            throw new InvalidGameActionException(e.getMessage(), e);
        }
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
            throw new MatchNotFoundException(matchId.toString());
        }
        return match;
    }

    private Card convertJsonCardToCard(JsonCard jsonCard) {
        try {
            return jsonCard.asCard();
        } catch (Exception e) {
            // Cualquier problema al convertir la carta (tipo inexistente, firma incorrecta, etc.)
            // se mapea a InvalidGameParametersException para mantener la semántica previa.
            throw new InvalidGameParametersException("Tipo de carta desconocido: " + jsonCard.getType(), e);
        }
    }

    private JsonCard convertCardToJsonCard(Card card) {
        return card.asJson();
    }
} 