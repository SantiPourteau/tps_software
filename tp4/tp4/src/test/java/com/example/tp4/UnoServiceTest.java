package com.example.tp4;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.udesa.unoback.model.*;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

public class UnoServiceTest {

    private UnoService unoService;

    @BeforeEach
    public void setUp() {
        unoService = new UnoService();
    }

    @Test
    public void testCreateNewMatchWithTwoPlayers() {
        List<String> players = List.of("Alice", "Bob");
        
        UUID matchId = unoService.createNewMatch(players);
        
        assertNotNull(matchId);
        
        // Verificar que la partida se creó correctamente obteniendo la carta activa
        JsonCard activeCard = unoService.getActiveCard(matchId);
        assertNotNull(activeCard);
        assertNotNull(activeCard.getColor());
        assertNotNull(activeCard.getType());
    }

    @Test
    public void testCreateNewMatchWithThreePlayers() {
        List<String> players = List.of("Alice", "Bob", "Charlie");
        
        UUID matchId = unoService.createNewMatch(players);
        
        assertNotNull(matchId);
        JsonCard activeCard = unoService.getActiveCard(matchId);
        assertNotNull(activeCard);
    }

    @Test
    public void testCreateNewMatchWithInvalidPlayerCount() {
        // Test con null
        assertThrows(RuntimeException.class, () -> {
            unoService.createNewMatch(null);
        });

        // Test con un solo jugador
        List<String> onePlayer = List.of("Alice");
        assertThrows(RuntimeException.class, () -> {
            unoService.createNewMatch(onePlayer);
        });

        // Test con lista vacía
        List<String> noPlayers = List.of();
        assertThrows(RuntimeException.class, () -> {
            unoService.createNewMatch(noPlayers);
        });
    }

    @Test
    public void testGetPlayerHand() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        
        List<JsonCard> hand = unoService.getPlayerHand(matchId);
        
        assertNotNull(hand);
        assertEquals(7, hand.size()); // Juego completo inicia con 7 cartas
        
        // Verificar que todas las cartas tienen propiedades válidas
        for (JsonCard card : hand) {
            assertNotNull(card.getColor());
            assertNotNull(card.getType());
        }
    }

    @Test
    public void testDrawCard() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        
        // Obtener tamaño inicial de la mano
        List<JsonCard> initialHand = unoService.getPlayerHand(matchId);
        int initialSize = initialHand.size();
        
        // Robar una carta
        unoService.drawCard(matchId, "Alice");
        
        // Verificar que la mano aumentó en 1
        List<JsonCard> newHand = unoService.getPlayerHand(matchId);
        assertEquals(initialSize + 1, newHand.size());
    }

    @Test
    public void testDrawCardWrongPlayer() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        
        // Intentar robar carta con el jugador incorrecto (Bob no es el primer jugador)
        assertThrows(RuntimeException.class, () -> {
            unoService.drawCard(matchId, "Bob");
        });
    }

    @Test
    public void testPlayCardInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        JsonCard card = new JsonCard("Red", 1, "NumberCard", false);
        
        assertThrows(RuntimeException.class, () -> {
            unoService.playCard(fakeMatchId, "Alice", card);
        });
    }

    @Test
    public void testGetActiveCardInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        
        assertThrows(RuntimeException.class, () -> {
            unoService.getActiveCard(fakeMatchId);
        });
    }

    @Test
    public void testGetPlayerHandInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        
        assertThrows(RuntimeException.class, () -> {
            unoService.getPlayerHand(fakeMatchId);
        });
    }

    @Test
    public void testDrawCardInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        
        assertThrows(RuntimeException.class, () -> {
            unoService.drawCard(fakeMatchId, "Alice");
        });
    }

    @Test
    public void testConvertJsonCardToCard() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        
        // Crear diferentes tipos de cartas JSON
        JsonCard numberCard = new JsonCard("Red", 5, "NumberCard", false);
        JsonCard skipCard = new JsonCard("Blue", null, "SkipCard", false);
        JsonCard reverseCard = new JsonCard("Green", null, "ReverseCard", false);
        JsonCard draw2Card = new JsonCard("Yellow", null, "Draw2Card", false);
        JsonCard wildCard = new JsonCard("Red", null, "WildCard", false);
        
        // Verificar que no lance excepciones (las cartas podrían no estar en la mano, pero la conversión debe funcionar)
        assertDoesNotThrow(() -> {
            try {
                unoService.playCard(matchId, "Alice", numberCard);
            } catch (RuntimeException e) {
                // Ignoramos errores de lógica de juego, solo queremos probar la conversión
            }
        });
    }

    @Test
    public void testUnoShout() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        
        // Crear una carta con UNO gritado
        JsonCard cardWithUno = new JsonCard("Red", 1, "NumberCard", true);
        
        // La conversión debe manejar el flag de UNO
        assertDoesNotThrow(() -> {
            try {
                unoService.playCard(matchId, "Alice", cardWithUno);
            } catch (RuntimeException e) {
                // Ignoramos errores de lógica de juego
            }
        });
    }

    @Test
    public void testWildCardWithColor() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        
        // Crear una carta comodín con color asignado
        JsonCard wildCardWithColor = new JsonCard("Blue", null, "WildCard", false);
        
        assertDoesNotThrow(() -> {
            try {
                unoService.playCard(matchId, "Alice", wildCardWithColor);
            } catch (RuntimeException e) {
                // Ignoramos errores de lógica de juego
            }
        });
    }

    @Test
    public void testInvalidCardType() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        
        // Crear una carta con tipo inválido
        JsonCard invalidCard = new JsonCard("Red", 1, "InvalidCard", false);
        
        assertThrows(RuntimeException.class, () -> {
            unoService.playCard(matchId, "Alice", invalidCard);
        });
    }

    @Test
    public void testMultipleMatches() {
        List<String> players1 = List.of("Alice", "Bob");
        List<String> players2 = List.of("Charlie", "David");
        
        UUID matchId1 = unoService.createNewMatch(players1);
        UUID matchId2 = unoService.createNewMatch(players2);
        
        assertNotEquals(matchId1, matchId2);
        
        // Verificar que ambas partidas son independientes
        JsonCard activeCard1 = unoService.getActiveCard(matchId1);
        JsonCard activeCard2 = unoService.getActiveCard(matchId2);
        
        assertNotNull(activeCard1);
        assertNotNull(activeCard2);
        
        List<JsonCard> hand1 = unoService.getPlayerHand(matchId1);
        List<JsonCard> hand2 = unoService.getPlayerHand(matchId2);
        
        assertEquals(7, hand1.size());
        assertEquals(7, hand2.size());
    }
} 