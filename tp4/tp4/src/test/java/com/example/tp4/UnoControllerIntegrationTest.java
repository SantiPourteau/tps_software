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
        lenient().when(dealer.fullDeck()).thenReturn(createTestDeck());
    }

    private List<Card> createTestDeck() {
        List<Card> deck = new ArrayList<>();
        deck.add(new NumberCard("Red", 1));
        for (int player = 0; player < 3; player++) {
            for (int card = 0; card < 7; card++) {
                deck.add(new NumberCard("Blue", card + 1));
            }
        }
        for (int i = 0; i < 30; i++) {
            deck.add(new NumberCard("Green", (i % 9) + 1));
        }
        return deck;
    }

    @Test
    public void test01CreateNewMatchEndpoint() throws Exception {
        MvcResult result = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        
        String response = result.getResponse().getContentAsString();
        assertNotNull(response);
        assertTrue(response.length() > 10); // UUID validation
    }

    @Test
    public void test02CreateMatchWithInvalidParameters() throws Exception {
        // Test with empty players list
        mockMvc.perform(post("/newmatch")
                .param("players", ""))
                .andExpect(status().isBadRequest());

        // Test with single player
        mockMvc.perform(post("/newmatch")
                .param("players", "A"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test03InvalidMatchIdHandling() throws Exception {
        String fakeMatchId = "00000000-0000-0000-0000-000000000000";

        mockMvc.perform(get("/activecard/" + fakeMatchId))
                .andExpect(status().isNotFound());

        mockMvc.perform(post("/draw/" + fakeMatchId + "/A"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void test04PlayCardEndpoint() throws Exception {
        // Create new match
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Play a valid card
        JsonCard validCard = new JsonCard("Blue", 1, "NumberCard", false);
        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(validCard)))
                .andExpect(status().isOk());
    }

    @Test
    public void test05PlayCardWithInvalidData() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Test with invalid card type
        JsonCard invalidCard = new JsonCard("Red", 1, "InvalidCardType", false);
        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(invalidCard)))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test06DrawCardEndpoint() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        mockMvc.perform(post("/draw/" + matchId + "/A"))
                .andExpect(status().isOk());
    }

    @Test
    public void test07DrawCardWithWrongPlayer() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        mockMvc.perform(post("/draw/" + matchId + "/B"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test08ActiveCardEndpoint() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        MvcResult result = mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        JsonCard activeCard = objectMapper.readValue(result.getResponse().getContentAsString(), JsonCard.class);
        assertNotNull(activeCard);
        assertNotNull(activeCard.getColor());
        assertNotNull(activeCard.getType());
    }

    @Test
    public void test09PlayerHandEndpoint() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        MvcResult result = mockMvc.perform(get("/playerhand/" + matchId))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        JsonCard[] hand = objectMapper.readValue(result.getResponse().getContentAsString(), JsonCard[].class);
        assertEquals(7, hand.length);
    }

    @Test
    public void test10ErrorHandling() throws Exception {
        // Test with invalid JSON
        mockMvc.perform(post("/newmatch")
                .contentType(MediaType.APPLICATION_JSON)
                .content("invalid json"))
                .andExpect(status().isBadRequest());

        // Test with invalid endpoint
        mockMvc.perform(get("/nonexistent"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void test11ContentTypeValidation() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Test play endpoint with wrong content type
        JsonCard card = new JsonCard("Blue", 1, "NumberCard", false);
        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.TEXT_PLAIN) // Wrong content type
                .content(objectMapper.writeValueAsString(card)))
                .andExpect(status().isUnsupportedMediaType());

        // Test draw endpoint with content type when not needed
        mockMvc.perform(post("/draw/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{}"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test12RequestBodyValidation() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Test play endpoint with malformed JSON
        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{invalid json"))
                .andExpect(status().isBadRequest());

        // Test play endpoint with missing required fields
        mockMvc.perform(post("/play/" + matchId + "/A")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"color\": \"Red\"}")) // Missing type
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test13PathVariableValidation() throws Exception {
        // Test with invalid UUID format
        mockMvc.perform(get("/activecard/invalid-uuid"))
                .andExpect(status().isBadRequest());

        // Test with empty player name
        mockMvc.perform(post("/draw/00000000-0000-0000-0000-000000000000/"))
                .andExpect(status().isNotFound());

        // Test with invalid player name format
        mockMvc.perform(post("/draw/00000000-0000-0000-0000-000000000000/player with spaces"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void test14ResponseStructureValidation() throws Exception {
        MvcResult createResult = mockMvc.perform(post("/newmatch")
                .param("players", "A")
                .param("players", "B"))
                .andExpect(status().isOk())
                .andReturn();
        String matchId = createResult.getResponse().getContentAsString().replace("\"", "");

        // Verify active card response structure
        MvcResult activeCardResult = mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andReturn();
        JsonCard activeCard = objectMapper.readValue(activeCardResult.getResponse().getContentAsString(), JsonCard.class);
        assertNotNull(activeCard.getColor());
        assertNotNull(activeCard.getType());
        assertNotNull(activeCard.isShout());

        // Verify player hand response structure
        MvcResult handResult = mockMvc.perform(get("/playerhand/" + matchId))
                .andExpect(status().isOk())
                .andReturn();
        JsonCard[] hand = objectMapper.readValue(handResult.getResponse().getContentAsString(), JsonCard[].class);
        for (JsonCard card : hand) {
            assertNotNull(card.getColor());
            assertNotNull(card.getType());
            assertNotNull(card.isShout());
        }
    }
} 