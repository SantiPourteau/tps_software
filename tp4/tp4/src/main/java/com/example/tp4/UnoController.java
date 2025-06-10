package com.example.tp4;

import org.springframework.beans.factory.annotation.Autowired;
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
        try {
            UUID matchId = unoService.createNewMatch(players);
            return ResponseEntity.ok().body(matchId.toString());
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @PostMapping("play/{matchId}/{player}")
    public ResponseEntity<?> play(@PathVariable UUID matchId, @PathVariable String player, @RequestBody JsonCard card) {
        try {
            unoService.playCard(matchId, player, card);
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @PostMapping("draw/{matchId}/{player}")
    public ResponseEntity<?> drawCard(@PathVariable UUID matchId, @PathVariable String player) {
        try {
            unoService.drawCard(matchId, player);
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @GetMapping("activecard/{matchId}")
    public ResponseEntity<?> activeCard(@PathVariable UUID matchId) {
        try {
            JsonCard activeCard = unoService.getActiveCard(matchId);
            return ResponseEntity.ok().body(activeCard);
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @GetMapping("playerhand/{matchId}")
    public ResponseEntity<?> playerHand(@PathVariable UUID matchId) {
        try {
            List<JsonCard> hand = unoService.getPlayerHand(matchId);
            return ResponseEntity.ok().body(hand);
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }
} 