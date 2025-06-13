package com.example.tp4;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.udesa.unoback.model.*;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@SpringBootTest
@AutoConfigureMockMvc
public class UnoControllerIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private Dealer dealer;

    @BeforeEach
    public void setUp() {
        // Configurar el dealer mock para devolver un mazo predecible por defecto
        lenient().when(dealer.fullDeck()).thenReturn(createTestDeck());
    }

    private void configureWinningScenario() {
        // Configurar un mazo donde el jugador A puede ganar rápidamente
        List<Card> winningDeck = new ArrayList<>();
        
        // Carta activa inicial
        winningDeck.add(new NumberCard("Red", 1));
        
        // Jugador A obtiene solo 1 carta que puede jugar
        winningDeck.add(new NumberCard("Red", 2));
        
        // Jugador B obtiene 7 cartas normales
        for (int i = 0; i < 7; i++) {
            winningDeck.add(new NumberCard("Blue", i + 1));
        }
        
        // Cartas adicionales para el mazo
        for (int i = 0; i < 30; i++) {
            winningDeck.add(new NumberCard("Green", (i % 9) + 1));
        }
        
        when(dealer.fullDeck()).thenReturn(winningDeck);
    }

    private List<Card> createTestDeck() {
        // Mazo predecible para tests del controller que soporte 3 jugadores
        List<Card> deck = new ArrayList<>();
        
        // Carta activa inicial
        deck.add(new NumberCard("Red", 1));
        
        // Cartas para 3 jugadores (7 cartas cada uno = 21 cartas)
        for (int player = 0; player < 3; player++) {
            for (int card = 0; card < 7; card++) {
                deck.add(new NumberCard("Blue", card + 1));
            }
        }
        
        // Cartas adicionales para el mazo de robo
        for (int i = 0; i < 30; i++) {
            deck.add(new NumberCard("Green", (i % 9) + 1));
        }
        
        return deck;
    }

    @Test
    public void test01CreateNewMatch() throws Exception {
        MvcResult result = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        
        String response = result.getResponse().getContentAsString();
        assertNotNull(response);
        assertTrue(response.length() > 10); // UUID tiene más de 10 caracteres
    }

    @Test
    public void test02FullGameFlow() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Obtener carta activa
        mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andExpect(content().string(org.hamcrest.Matchers.not(org.hamcrest.Matchers.emptyString())));

        // Obtener mano del jugador en turno
        mockMvc.perform(get("/playerhand/" + matchId))
                .andExpect(status().isOk())
                .andExpect(content().string(org.hamcrest.Matchers.not(org.hamcrest.Matchers.emptyString())));

        // Robar una carta
        mockMvc.perform(post("/draw/" + matchId + "/A"))
                .andExpect(status().isOk());
    }

    @Test
    public void test03InvalidMatch() throws Exception {
        String fakeMatchId = "00000000-0000-0000-0000-000000000000";

        // Intentar obtener carta activa de partida inexistente - debería ser 404 (Not Found)
        mockMvc.perform(get("/activecard/" + fakeMatchId))
                .andExpect(status().isNotFound());

        // Intentar robar carta de partida inexistente - debería ser 404 (Not Found)
        mockMvc.perform(post("/draw/" + fakeMatchId + "/A"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void test04CreateMatchWithInsufficientPlayers() throws Exception {
        mockMvc.perform(post("/newmatch")
                .param("players", "A"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test05CreateMatchWithThreePlayers() throws Exception {
        MvcResult result = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B")
                .param("players", "C"))
                .andExpect(status().isOk())
                .andReturn();
        
        String response = result.getResponse().getContentAsString();
        assertNotNull(response);
    }

    @Test
    public void test06PlayValidCard() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Como conocemos el mazo, sabemos que la carta activa es Red 1
        // y el jugador A tiene Blue 1 (que puede jugar porque mismo número)
        JsonCard validCard = new JsonCard("Blue", 1, "NumberCard", false);

        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(validCard)))
                .andExpect(status().isOk());

        // Verificar que la carta activa cambió
        MvcResult activeCardResult = mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andReturn();
        
        JsonCard newActiveCard = objectMapper.readValue(activeCardResult.getResponse().getContentAsString(), JsonCard.class);
        assertEquals("Blue", newActiveCard.getColor());
        assertEquals(1, newActiveCard.getNumber());
    }

    @Test
    public void test07PlayInvalidCard() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Intentar jugar una carta que no está en la mano
        JsonCard invalidCard = new JsonCard("Purple", 15, "NumberCard", false);

        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(invalidCard)))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test08WrongPlayerTurn() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Intentar que el jugador B juegue primero (debería ser el turno de A)
        mockMvc.perform(post("/draw/" + matchId + "/B"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test09EmptyPlayersParam() throws Exception {
        // Test con lista vacía explícita
        mockMvc.perform(post("/newmatch")
                .param("players", ""))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test10ActiveCardStructure() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Obtener carta activa y verificar estructura
        MvcResult activeCardResult = mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andReturn();
        
        JsonCard activeCard = objectMapper.readValue(activeCardResult.getResponse().getContentAsString(), JsonCard.class);
        
        assertNotNull(activeCard.getColor());
        assertNotNull(activeCard.getType());
        assertFalse(activeCard.isShout()); // La carta inicial no debería tener UNO gritado
    }

    @Test
    public void test11PlayerHandStructure() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Obtener mano del jugador y verificar estructura
        MvcResult playerHandResult = mockMvc.perform(get("/playerhand/" + matchId))
                .andExpect(status().isOk())
                .andReturn();
        
        JsonCard[] hand = objectMapper.readValue(playerHandResult.getResponse().getContentAsString(), JsonCard[].class);
        
        assertEquals(7, hand.length); // Un juego completo inicia con 7 cartas
        for (JsonCard card : hand) {
            assertNotNull(card.getColor());
            assertNotNull(card.getType());
        }
    }

    @Test
    public void test12PlayOnFinishedMatch() throws Exception {
        configureWinningScenario();
        
        // Crear nueva partida con escenario de victoria rápida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Jugador A juega su única carta para ganar
        JsonCard winningCard = new JsonCard("Red", 2, "NumberCard", false);

        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(winningCard)))
                .andExpect(status().isOk());

        // Intentar jugar en la partida ya terminada debería fallar
        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(winningCard)))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test13PlayWithNonExistentPlayer() throws Exception {
        // Crear nueva partida con jugadores A y B
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Intentar jugar con jugador C que no existe en la partida
        JsonCard validCard = new JsonCard("Blue", 1, "NumberCard", false);

        mockMvc.perform(post("/play/" + matchId + "/C")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(validCard)))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test14DrawWithNonExistentPlayer() throws Exception {
        // Crear nueva partida con jugadores A y B
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Intentar robar carta con jugador C que no existe
        mockMvc.perform(post("/draw/" + matchId + "/C"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test15PlayCardThatDoesNotMatchActiveCard() throws Exception {
        // Crear nueva partida (carta activa es Red 1)
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Intentar jugar una carta que no coincide ni por color ni por número
        JsonCard invalidCard = new JsonCard("Green", 5, "NumberCard", false);

        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(invalidCard)))
                .andExpect(status().isBadRequest());
    }

    @Test 
    public void test16GetPlayerHandConsistency() throws Exception {
        // Crear nueva partida con 3 jugadores
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B")
                .param("players", "C"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Verificar que playerhand devuelve la mano del jugador en turno (debería ser A)
        MvcResult playerHandResult = mockMvc.perform(get("/playerhand/" + matchId))
                .andExpect(status().isOk())
                .andReturn();
        
        JsonCard[] hand = objectMapper.readValue(playerHandResult.getResponse().getContentAsString(), JsonCard[].class);
        assertEquals(7, hand.length, "Player in turn should have exactly 7 cards");
        
        // Todas las cartas deberían ser Blue (según nuestro mock)
        for (JsonCard card : hand) {
            assertEquals("Blue", card.getColor());
        }
    }

    @Test
    public void test17InitialGameStateConsistency() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B")
                .param("players", "C"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Verificar que la carta activa existe
        mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andExpect(content().string(org.hamcrest.Matchers.not(org.hamcrest.Matchers.emptyString())));

        // Verificar que el jugador en turno tiene exactamente 7 cartas
        MvcResult playerHandResult = mockMvc.perform(get("/playerhand/" + matchId))
                .andExpect(status().isOk())
                .andReturn();
        
        JsonCard[] hand = objectMapper.readValue(playerHandResult.getResponse().getContentAsString(), JsonCard[].class);
        assertEquals(7, hand.length, "Player in turn should have exactly 7 cards");
    }

    @Test
    public void test18PlayInvalidCardType() throws Exception {
        // Crear nueva partida
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Intentar jugar una carta con tipo inválido
        JsonCard invalidCard = new JsonCard("Red", 1, "InvalidCardType", false);

        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(invalidCard)))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test19MultipleInvalidPlayerOperations() throws Exception {
        // Crear nueva partida con jugadores A y B
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Test 1: Jugador inexistente intenta jugar carta
        JsonCard validCard = new JsonCard("Blue", 1, "NumberCard", false);

        mockMvc.perform(post("/play/" + matchId + "/NonExistentPlayer")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(validCard)))
                .andExpect(status().isBadRequest());

        // Test 2: Jugador inexistente intenta robar carta
        mockMvc.perform(post("/draw/" + matchId + "/NonExistentPlayer"))
                .andExpect(status().isBadRequest());
    }
} 