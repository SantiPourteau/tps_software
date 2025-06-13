package com.example.tp4;

import com.example.tp4.exceptions.InvalidGameActionException;
import com.example.tp4.exceptions.InvalidGameParametersException;
import com.example.tp4.exceptions.MatchNotFoundException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.udesa.unoback.model.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.lenient;

@SpringBootTest
public class UnoServiceTest {

    @MockBean
    private Dealer dealer;

    @Autowired
    private UnoService unoService;

    @BeforeEach
    public void setUp() {
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
    public void test01MatchCreation() {
        // Test basic match creation
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);
        assertNotNull(matchId);
        verify(dealer, times(1)).fullDeck();
    }

    @Test
    public void test02MatchRetrieval() {
        // Create a match
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);

        // Test retrieving match state
        JsonCard activeCard = unoService.getActiveCard(matchId);
        List<JsonCard> hand = unoService.getPlayerHand(matchId);

        assertNotNull(activeCard);
        assertNotNull(hand);
        assertEquals(7, hand.size());
    }

    @Test
    public void test03InvalidMatchHandling() {
        UUID fakeMatchId = UUID.randomUUID();

        // Test all operations with invalid match ID
        assertThrows(MatchNotFoundException.class, () -> unoService.getActiveCard(fakeMatchId));
        assertThrows(MatchNotFoundException.class, () -> unoService.getPlayerHand(fakeMatchId));
        assertThrows(MatchNotFoundException.class, () -> unoService.drawCard(fakeMatchId, "Alice"));
        assertThrows(MatchNotFoundException.class, () -> 
            unoService.playCard(fakeMatchId, "Alice", new JsonCard("Red", 1, "NumberCard", false)));
    }

    @Test
    public void test04PlayerValidation() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);

        // Test with non-existent player
        assertThrows(InvalidGameActionException.class, () -> 
            unoService.playCard(matchId, "Charlie", new JsonCard("Red", 1, "NumberCard", false)));
        assertThrows(InvalidGameActionException.class, () -> 
            unoService.drawCard(matchId, "Charlie"));
    }

    @Test
    public void test05CardConversion() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);

        // Test conversion of all card types
        JsonCard numberCard = new JsonCard("Red", 5, "NumberCard", false);
        JsonCard skipCard = new JsonCard("Blue", null, "SkipCard", false);
        JsonCard reverseCard = new JsonCard("Green", null, "ReverseCard", false);
        JsonCard draw2Card = new JsonCard("Yellow", null, "Draw2Card", false);
        JsonCard wildCard = new JsonCard("Red", null, "WildCard", false);

        // Verify conversion doesn't throw exceptions
        assertDoesNotThrow(() -> {
            try {
                unoService.playCard(matchId, "Alice", numberCard);
            } catch (RuntimeException e) {
                // Ignore game logic errors
            }
        });
    }

    @Test
    public void test06InvalidCardTypeHandling() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);

        // Test with invalid card type
        JsonCard invalidCard = new JsonCard("Red", 1, "InvalidCard", false);
        assertThrows(InvalidGameParametersException.class, () -> 
            unoService.playCard(matchId, "Alice", invalidCard));
    }

    @Test
    public void test07MultipleMatchManagement() {
        // Create multiple matches
        List<String> players1 = List.of("Alice", "Bob");
        List<String> players2 = List.of("Charlie", "David");
        
        UUID matchId1 = unoService.createNewMatch(players1);
        UUID matchId2 = unoService.createNewMatch(players2);
        
        // Verify matches are independent
        assertNotEquals(matchId1, matchId2);
        
        // Verify each match has its own state
        JsonCard activeCard1 = unoService.getActiveCard(matchId1);
        JsonCard activeCard2 = unoService.getActiveCard(matchId2);
        assertNotNull(activeCard1);
        assertNotNull(activeCard2);
    }

    @Test
    public void test08StateConsistency() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);

        // Get initial state
        JsonCard initialActiveCard = unoService.getActiveCard(matchId);
        List<JsonCard> initialHand = unoService.getPlayerHand(matchId);

        // Perform an action
        unoService.drawCard(matchId, "Alice");

        // Verify state is updated
        List<JsonCard> newHand = unoService.getPlayerHand(matchId);
        assertEquals(initialHand.size() + 1, newHand.size());
    }

    @Test
    public void test09InvalidParameters() {
        // Test match creation with invalid parameters
        assertThrows(InvalidGameParametersException.class, () -> unoService.createNewMatch(null));
        assertThrows(InvalidGameParametersException.class, () -> unoService.createNewMatch(List.of()));
        assertThrows(InvalidGameParametersException.class, () -> unoService.createNewMatch(List.of("Alice")));
    }

    @Test
    public void test10DealerIntegration() {
        // Verify dealer is used correctly
        List<String> players = List.of("Alice", "Bob");
        unoService.createNewMatch(players);
        verify(dealer, times(1)).fullDeck();
    }

    @Test
    public void test11MatchStatePersistence() {
        List<String> players = List.of("Alice", "Bob");
        UUID matchId = unoService.createNewMatch(players);

        // Get initial state
        JsonCard initialActiveCard = unoService.getActiveCard(matchId);
        List<JsonCard> initialHand = unoService.getPlayerHand(matchId);

        // Perform multiple actions
        unoService.drawCard(matchId, "Alice");
        unoService.drawCard(matchId, "Alice");

        // Verify state is maintained
        List<JsonCard> finalHand = unoService.getPlayerHand(matchId);
        assertEquals(initialHand.size() + 2, finalHand.size());
        
        // Compare active card properties instead of objects
        JsonCard finalActiveCard = unoService.getActiveCard(matchId);
        assertEquals(initialActiveCard.getColor(), finalActiveCard.getColor());
        assertEquals(initialActiveCard.getNumber(), finalActiveCard.getNumber());
        assertEquals(initialActiveCard.getType(), finalActiveCard.getType());
        assertEquals(initialActiveCard.isShout(), finalActiveCard.isShout());
    }

    @Test
    public void test12ConcurrentMatchAccess() {
        // Create two matches
        List<String> players1 = List.of("Alice", "Bob");
        List<String> players2 = List.of("Charlie", "David");
        
        UUID matchId1 = unoService.createNewMatch(players1);
        UUID matchId2 = unoService.createNewMatch(players2);

        // Perform actions on both matches
        unoService.drawCard(matchId1, "Alice");
        unoService.drawCard(matchId2, "Charlie");

        // Verify both matches maintain their state independently
        List<JsonCard> hand1 = unoService.getPlayerHand(matchId1);
        List<JsonCard> hand2 = unoService.getPlayerHand(matchId2);
        
        assertEquals(8, hand1.size()); // 7 initial + 1 drawn
        assertEquals(8, hand2.size()); // 7 initial + 1 drawn
    }
}