package com.example.tp4;

import com.example.tp4.exceptions.InvalidGameActionException;
import com.example.tp4.exceptions.InvalidGameParametersException;
import com.example.tp4.exceptions.MatchNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.udesa.unoback.model.JsonCard;

import java.util.List;
import java.util.UUID;

@RestController
public class UnoController {

    @Autowired
    private UnoService unoService;

    @PostMapping("newmatch")
    public ResponseEntity<?> newMatch(@RequestParam List<String> players) {
        UUID matchId = unoService.createNewMatch(players);
        return ResponseEntity.ok().body(matchId.toString());
    }

    @PostMapping("play/{matchId}/{player}")
    public ResponseEntity<?> play(@PathVariable UUID matchId, @PathVariable String player, @RequestBody JsonCard card) {
        unoService.playCard(matchId, player, card);
        return ResponseEntity.ok().build();
    }

    @PostMapping("draw/{matchId}/{player}")
    public ResponseEntity<?> drawCard(@PathVariable UUID matchId, @PathVariable String player) {
        unoService.drawCard(matchId, player);
        return ResponseEntity.ok().build();
    }

    @GetMapping("activecard/{matchId}")
    public ResponseEntity<?> activeCard(@PathVariable UUID matchId) {
        JsonCard activeCard = unoService.getActiveCard(matchId);
        return ResponseEntity.ok().body(activeCard);
    }

    @GetMapping("playerhand/{matchId}")
    public ResponseEntity<?> playerHand(@PathVariable UUID matchId) {
        List<JsonCard> hand = unoService.getPlayerHand(matchId);
        return ResponseEntity.ok().body(hand);
    }

    // Handler de excepciones para evitar repetir try/catch en cada método
    @ExceptionHandler(MatchNotFoundException.class)
    public ResponseEntity<String> handleMatchNotFound(MatchNotFoundException e) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(e.getMessage());
    }

    @ExceptionHandler(InvalidGameActionException.class)
    public ResponseEntity<String> handleInvalidGameAction(InvalidGameActionException e) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(e.getMessage());
    }

    @ExceptionHandler(InvalidGameParametersException.class)
    public ResponseEntity<String> handleInvalidGameParameters(InvalidGameParametersException e) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(e.getMessage());
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> handleGenericException(Exception e) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error interno del servidor");
    }
} 