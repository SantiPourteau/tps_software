package com.example.tp4;

import org.springframework.stereotype.Service;
import org.udesa.unoback.model.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
public class Dealer {

    public List<Card> fullDeck() {
        List<Card> deck = new ArrayList<>();
        String[] colors = {"Red", "Blue", "Green", "Yellow"};
        
        // Cartas numéricas (0-9) - 2 de cada número por color (excepto 0)
        for (String color : colors) {
            deck.add(new NumberCard(color, 0)); // Solo una carta 0 por color
            for (int number = 1; number <= 9; number++) {
                deck.add(new NumberCard(color, number));
                deck.add(new NumberCard(color, number));
            }
        }
        
        // Cartas especiales - 2 de cada tipo por color
        for (String color : colors) {
            deck.add(new SkipCard(color));
            deck.add(new SkipCard(color));
            deck.add(new ReverseCard(color));
            deck.add(new ReverseCard(color));
            deck.add(new Draw2Card(color));
            deck.add(new Draw2Card(color));
        }
        
        // Cartas comodín - 4 de cada tipo
        for (int i = 0; i < 4; i++) {
            deck.add(new WildCard());
        }
        
        // Mezclar el mazo
        Collections.shuffle(deck);
        return deck;
    }
} 