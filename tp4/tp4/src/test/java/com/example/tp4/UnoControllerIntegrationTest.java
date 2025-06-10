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
        // Configurar el dealer mock para devolver un mazo predecible
        lenient().when(dealer.fullDeck()).thenReturn(createTestDeck());
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
    public void testCreateNewMatch() {
        String url = "http://localhost:" + port + "/newmatch?players=A&players=B";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        
        assertEquals(200, response.getStatusCode().value());
        assertNotNull(response.getBody());
        assertTrue(response.getBody().length() > 10); // UUID tiene más de 10 caracteres
    }

    @Test
    public void testFullGameFlow() {
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
    public void testInvalidMatch() {
        String fakeMatchId = "00000000-0000-0000-0000-000000000000";

        // Intentar obtener carta activa de partida inexistente
        String activeCardUrl = "http://localhost:" + port + "/activecard/" + fakeMatchId;
        ResponseEntity<String> activeCardResponse = restTemplate.getForEntity(activeCardUrl, String.class);
        assertEquals(400, activeCardResponse.getStatusCode().value());

        // Intentar robar carta de partida inexistente
        String drawUrl = "http://localhost:" + port + "/draw/" + fakeMatchId + "/A";
        ResponseEntity<String> drawResponse = restTemplate.postForEntity(drawUrl, null, String.class);
        assertEquals(400, drawResponse.getStatusCode().value());
    }

    @Test
    public void testCreateMatchWithInsufficientPlayers() {
        String url = "http://localhost:" + port + "/newmatch?players=A";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        assertEquals(400, response.getStatusCode().value());
    }

    @Test
    public void testCreateMatchWithThreePlayers() {
        String url = "http://localhost:" + port + "/newmatch?players=A&players=B&players=C";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        assertEquals(200, response.getStatusCode().value());
        assertNotNull(response.getBody());
    }

    @Test
    public void testPlayValidCard() throws Exception {
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
    public void testPlayInvalidCard() throws Exception {
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
    public void testWrongPlayerTurn() throws Exception {
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
    public void testEmptyPlayersParam() {
        String url = "http://localhost:" + port + "/newmatch";
        ResponseEntity<String> response = restTemplate.postForEntity(url, null, String.class);
        assertEquals(400, response.getStatusCode().value());
    }

    @Test
    public void testActiveCardStructure() throws Exception {
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
    public void testPlayerHandStructure() throws Exception {
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
} 