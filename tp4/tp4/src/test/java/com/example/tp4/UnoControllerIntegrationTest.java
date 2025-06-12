package com.example.tp4;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.udesa.unoback.model.*;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.lenient;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class UnoControllerIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private TestRestTemplate restTemplate;

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
    public void test01CreateNewMatch() {
        String url = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        
        assertEquals(200, response.getStatusCode().value());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().length() > 10); // UUID tiene más de 10 caracteres
    }

    @Test
    public void test02FullGameFlow() {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        
        assertEquals(200, createResponse.getStatusCode().value());
        String matchId = createResponse.getBody().replace("\"", "");

        // Obtener carta activa
        String activeCardUrl = "http://localhost:" + port + "/activecard/" + matchId;
        ResponseEntity<String> activeCardResponse = restTemplate.getForEntity(activeCardUrl, String.class);
        assertEquals(200, activeCardResponse.getStatusCode().value());
        assertNotNull(activeCardResponse.getBody());

        // Obtener mano del jugador en turno
        String playerHandUrl = "http://localhost:" + port + "/playerhand/" + matchId;
        ResponseEntity<String> playerHandResponse = restTemplate.getForEntity(playerHandUrl, String.class);
        assertEquals(200, playerHandResponse.getStatusCode().value());
        assertNotNull(playerHandResponse.getBody());

        // Robar una carta
        String drawUrl = "http://localhost:" + port + "/draw/" + matchId + "/A";
        ResponseEntity<String> drawResponse = restTemplate.postForEntity(drawUrl, null, String.class);
        assertEquals(200, drawResponse.getStatusCode().value());
    }

    @Test
    public void test03InvalidMatch() {
        String fakeMatchId = "00000000-0000-0000-0000-000000000000";

        // Intentar obtener carta activa de partida inexistente - debería ser 404 (Not Found)
        String activeCardUrl = "http://localhost:" + port + "/activecard/" + fakeMatchId;
        ResponseEntity<String> activeCardResponse = restTemplate.getForEntity(activeCardUrl, String.class);
        assertEquals(404, activeCardResponse.getStatusCode().value());

        // Intentar robar carta de partida inexistente - debería ser 404 (Not Found)
        String drawUrl = "http://localhost:" + port + "/draw/" + fakeMatchId + "/A";
        ResponseEntity<String> drawResponse = restTemplate.postForEntity(drawUrl, null, String.class);
        assertEquals(404, drawResponse.getStatusCode().value());
    }

    @Test
    public void test04CreateMatchWithInsufficientPlayers() {
        String url = "http://localhost:" + port + "/newmatch?players=A";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        assertEquals(400, response.getStatusCode().value());
    }

    @Test
    public void test05CreateMatchWithThreePlayers() {
        String url = "http://localhost:" + port + "/newmatch?players=A&players=B&players=C";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        assertEquals(200, response.getStatusCode().value());
        assertNotNull(response.getBody());
    }

    @Test
    public void test06PlayValidCard() throws Exception {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Como conocemos el mazo, sabemos que la carta activa es Red 1
        // y el jugador A tiene Blue 1 (que puede jugar porque mismo número)
        JsonCard validCard = new JsonCard("Blue", 1, "NumberCard", false);
        
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<String> request = new HttpEntity<>(objectMapper.writeValueAsString(validCard), headers);

        String playUrl = "http://localhost:" + port + "/play/" + matchId + "/A";
        ResponseEntity<String> playResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(200, playResponse.getStatusCode().value());

        // Verificar que la carta activa cambió
        String activeCardUrl = "http://localhost:" + port + "/activecard/" + matchId;
        ResponseEntity<String> activeCardResponse = restTemplate.getForEntity(activeCardUrl, String.class);
        JsonCard newActiveCard = objectMapper.readValue(activeCardResponse.getBody(), JsonCard.class);
        assertEquals("Blue", newActiveCard.getColor());
        assertEquals(1, newActiveCard.getNumber());
    }

    @Test
    public void test07PlayInvalidCard() throws Exception {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Intentar jugar una carta que no está en la mano
        JsonCard invalidCard = new JsonCard("Purple", 15, "NumberCard", false);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<String> request = new HttpEntity<>(objectMapper.writeValueAsString(invalidCard), headers);

        String playUrl = "http://localhost:" + port + "/play/" + matchId + "/A";
        ResponseEntity<String> playResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(400, playResponse.getStatusCode().value());
    }

    @Test
    public void test08WrongPlayerTurn() throws Exception {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Intentar que el jugador B juegue primero (debería ser el turno de A)
        String drawUrl = "http://localhost:" + port + "/draw/" + matchId + "/B";
        ResponseEntity<String> drawResponse = restTemplate.postForEntity(drawUrl, null, String.class);
        assertEquals(400, drawResponse.getStatusCode().value());
    }

    @Test
    public void test09EmptyPlayersParam() {
        // Test con lista vacía explícita
        String url = "http://localhost:" + port + "/newmatch?players=";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        assertEquals(400, response.getStatusCode().value());
    }

    @Test
    public void test10ActiveCardStructure() throws Exception {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Obtener carta activa y verificar estructura
        String activeCardUrl = "http://localhost:" + port + "/activecard/" + matchId;
        ResponseEntity<String> activeCardResponse = restTemplate.getForEntity(activeCardUrl, String.class);
        
        assertEquals(200, activeCardResponse.getStatusCode().value());
        JsonCard activeCard = objectMapper.readValue(activeCardResponse.getBody(), JsonCard.class);
        
        assertNotNull(activeCard.getColor());
        assertNotNull(activeCard.getType());
        assertFalse(activeCard.isShout()); // La carta inicial no debería tener UNO gritado
    }

    @Test
    public void test11PlayerHandStructure() throws Exception {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Obtener mano del jugador y verificar estructura
        String playerHandUrl = "http://localhost:" + port + "/playerhand/" + matchId;
        ResponseEntity<String> playerHandResponse = restTemplate.getForEntity(playerHandUrl, String.class);
        
        assertEquals(200, playerHandResponse.getStatusCode().value());
        JsonCard[] hand = objectMapper.readValue(playerHandResponse.getBody(), JsonCard[].class);
        
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
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Jugador A juega su única carta para ganar
        JsonCard winningCard = new JsonCard("Red", 2, "NumberCard", false);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<String> request = new HttpEntity<>(objectMapper.writeValueAsString(winningCard), headers);

        String playUrl = "http://localhost:" + port + "/play/" + matchId + "/A";
        ResponseEntity<String> playResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(200, playResponse.getStatusCode().value());

        // Intentar jugar en la partida ya terminada debería fallar
        ResponseEntity<String> secondPlayResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(400, secondPlayResponse.getStatusCode().value());
    }

    @Test
    public void test13PlayWithNonExistentPlayer() throws Exception {
        // Crear nueva partida con jugadores A y B
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Intentar jugar con jugador C que no existe en la partida
        JsonCard validCard = new JsonCard("Blue", 1, "NumberCard", false);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<String> request = new HttpEntity<>(objectMapper.writeValueAsString(validCard), headers);

        String playUrl = "http://localhost:" + port + "/play/" + matchId + "/C";
        ResponseEntity<String> playResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(400, playResponse.getStatusCode().value());
    }

    @Test
    public void test14DrawWithNonExistentPlayer() {
        // Crear nueva partida con jugadores A y B
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Intentar robar carta con jugador C que no existe
        String drawUrl = "http://localhost:" + port + "/draw/" + matchId + "/C";
        ResponseEntity<String> drawResponse = restTemplate.postForEntity(drawUrl, null, String.class);
        assertEquals(400, drawResponse.getStatusCode().value());
    }

    @Test
    public void test15PlayCardThatDoesNotMatchActiveCard() throws Exception {
        // Crear nueva partida (carta activa es Red 1)
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Intentar jugar una carta que no coincide ni por color ni por número
        JsonCard invalidCard = new JsonCard("Green", 5, "NumberCard", false);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<String> request = new HttpEntity<>(objectMapper.writeValueAsString(invalidCard), headers);

        String playUrl = "http://localhost:" + port + "/play/" + matchId + "/A";
        ResponseEntity<String> playResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(400, playResponse.getStatusCode().value());
    }

    @Test 
    public void test16GetPlayerHandConsistency() throws Exception {
        // Crear nueva partida con 3 jugadores
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B&players=C";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Verificar que playerhand devuelve la mano del jugador en turno (debería ser A)
        String playerHandUrl = "http://localhost:" + port + "/playerhand/" + matchId;
        ResponseEntity<String> playerHandResponse = restTemplate.getForEntity(playerHandUrl, String.class);
        assertEquals(200, playerHandResponse.getStatusCode().value());
        
        JsonCard[] hand = objectMapper.readValue(playerHandResponse.getBody(), JsonCard[].class);
        assertEquals(7, hand.length, "Player in turn should have exactly 7 cards");
        
        // Todas las cartas deberían ser Blue (según nuestro mock)
        for (JsonCard card : hand) {
            assertEquals("Blue", card.getColor());
        }
    }

    @Test
    public void test17InitialGameStateConsistency() throws Exception {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B&players=C";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Verificar que la carta activa existe
        String activeCardUrl = "http://localhost:" + port + "/activecard/" + matchId;
        ResponseEntity<String> activeCardResponse = restTemplate.getForEntity(activeCardUrl, String.class);
        assertEquals(200, activeCardResponse.getStatusCode().value());
        assertNotNull(activeCardResponse.getBody());

        // Verificar que el jugador en turno tiene exactamente 7 cartas
        String playerHandUrl = "http://localhost:" + port + "/playerhand/" + matchId;
        ResponseEntity<String> playerHandResponse = restTemplate.getForEntity(playerHandUrl, String.class);
        assertEquals(200, playerHandResponse.getStatusCode().value());
        
        JsonCard[] hand = objectMapper.readValue(playerHandResponse.getBody(), JsonCard[].class);
        assertEquals(7, hand.length, "Player in turn should have exactly 7 cards");
    }

    @Test
    public void test18PlayInvalidCardType() throws Exception {
        // Crear nueva partida
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Intentar jugar una carta con tipo inválido
        JsonCard invalidCard = new JsonCard("Red", 1, "InvalidCardType", false);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<String> request = new HttpEntity<>(objectMapper.writeValueAsString(invalidCard), headers);

        String playUrl = "http://localhost:" + port + "/play/" + matchId + "/A";
        ResponseEntity<String> playResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(400, playResponse.getStatusCode().value());
    }

    @Test
    public void test19MultipleInvalidPlayerOperations() throws Exception {
        // Crear nueva partida con jugadores A y B
        String createUrl = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> createResponse = restTemplate.postForEntity(createUrl, null, String.class);
        String matchId = createResponse.getBody().replace("\"", "");

        // Test 1: Jugador inexistente intenta jugar carta
        JsonCard validCard = new JsonCard("Blue", 1, "NumberCard", false);
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<String> request = new HttpEntity<>(objectMapper.writeValueAsString(validCard), headers);

        String playUrl = "http://localhost:" + port + "/play/" + matchId + "/NonExistentPlayer";
        ResponseEntity<String> playResponse = restTemplate.postForEntity(playUrl, request, String.class);
        assertEquals(400, playResponse.getStatusCode().value());

        // Test 2: Jugador inexistente intenta robar carta
        String drawUrl = "http://localhost:" + port + "/draw/" + matchId + "/NonExistentPlayer";
        ResponseEntity<String> drawResponse = restTemplate.postForEntity(drawUrl, null, String.class);
        assertEquals(400, drawResponse.getStatusCode().value());
    }
} 