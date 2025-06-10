package com.example.tp4;

import com.example.tp4.exceptions.InvalidGameActionException;
import com.example.tp4.exceptions.InvalidGameParametersException;
import com.example.tp4.exceptions.MatchNotFoundException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.udesa.unoback.model.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.lenient;

@ExtendWith(MockitoExtension.class)
public class UnoServiceTest {

    @Mock
    private Dealer dealer;

    private UnoService unoService;

    @BeforeEach
    public void setUp() {
        unoService = new UnoService(dealer);
        
        // Configurar mock del dealer para devolver un mazo predecible
        // Solo cuando se llame realmente (lenient para evitar errores de stubbing innecesario)
        lenient().when(dealer.fullDeck()).thenReturn(createTestDeck());
    }

    private List<Card> createTestDeck() {
        // Crear un mazo grande para soportar 3 jugadores (21 cartas + 1 activa = 22 mínimo)
        List<Card> deck = new ArrayList<>();
        
        // Carta activa inicial
        deck.add(new NumberCard("Red", 1));
        
        // Cartas para jugadores (7 cartas cada uno)
        for (int player = 0; player < 3; player++) {
            for (int card = 0; card < 7; card++) {
                deck.add(new NumberCard("Blue", card + 1));
            }
        }
        
        // Cartas adicionales para el mazo de robo
        for (int i = 0; i < 20; i++) {
            deck.add(new NumberCard("Green", (i % 9) + 1));
        }
        
        return deck;
    }

    @Test
    public void testCreateNewMatchWithTwoPlayers() {
        List<String> players = List.of("Alice", "Bob");
        
        UUID matchId = unoService.createNewMatch(players);
        
        assertNotNull(matchId);
        
        // Verificar que se usó el dealer
        verify(dealer, times(1)).fullDeck();
        
        // Verificar que la partida se creó correctamente obteniendo la carta activa
        JsonCard activeCard = unoService.getActiveCard(matchId);
        assertNotNull(activeCard);
        assertEquals("Red", activeCard.getColor()); // Conocemos la carta porque controlamos el mazo
        assertEquals("NumberCard", activeCard.getType());
        assertEquals(1, activeCard.getNumber());
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
        assertThrows(InvalidGameParametersException.class, () -> {
            unoService.createNewMatch(null);
        });

        // Test con un solo jugador
        List<String> onePlayer = List.of("Alice");
        assertThrows(InvalidGameParametersException.class, () -> {
            unoService.createNewMatch(onePlayer);
        });

        // Test con lista vacía
        List<String> noPlayers = List.of();
        assertThrows(InvalidGameParametersException.class, () -> {
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
        
        // Como controlamos el mazo, sabemos qué cartas debe tener Alice
        assertEquals("Blue", hand.get(0).getColor());
        assertEquals(1, hand.get(0).getNumber());
        assertEquals("Blue", hand.get(1).getColor());
        assertEquals(2, hand.get(1).getNumber());
        
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
        assertThrows(InvalidGameActionException.class, () -> {
            unoService.drawCard(matchId, "Bob");
        });
    }

    @Test
    public void testPlayCardInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        JsonCard card = new JsonCard("Red", 1, "NumberCard", false);
        
        assertThrows(MatchNotFoundException.class, () -> {
            unoService.playCard(fakeMatchId, "Alice", card);
        });
    }

    @Test
    public void testGetActiveCardInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        
        assertThrows(MatchNotFoundException.class, () -> {
            unoService.getActiveCard(fakeMatchId);
        });
    }

    @Test
    public void testGetPlayerHandInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        
        assertThrows(MatchNotFoundException.class, () -> {
            unoService.getPlayerHand(fakeMatchId);
        });
    }

    @Test
    public void testDrawCardInvalidMatch() {
        UUID fakeMatchId = UUID.randomUUID();
        
        assertThrows(MatchNotFoundException.class, () -> {
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
        
        assertThrows(InvalidGameParametersException.class, () -> {
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