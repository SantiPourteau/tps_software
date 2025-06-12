package com.example.tp4;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.udesa.unoback.model.*;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

public class DealerTest {

    private Dealer dealer;

    @BeforeEach
    public void setUp() {
        dealer = new Dealer();
    }

    @Test
    public void testFullDeckSize() {
        List<Card> deck = dealer.fullDeck();
        
        // Un mazo completo de UNO: 76 numéricas + 8 Skip + 8 Reverse + 8 Draw2 + 4 Wild = 104 cartas
        assertEquals(104, deck.size());
    }

    @Test
    public void testFullDeckContainsCorrectCards() {
        List<Card> deck = dealer.fullDeck();
        
        // Contar cartas por tipo
        long numberCards = deck.stream().filter(card -> card instanceof NumberCard).count();
        long skipCards = deck.stream().filter(card -> card instanceof SkipCard).count();
        long reverseCards = deck.stream().filter(card -> card instanceof ReverseCard).count();
        long draw2Cards = deck.stream().filter(card -> card instanceof Draw2Card).count();
        long wildCards = deck.stream().filter(card -> card instanceof WildCard).count();
        
        // Verificar cantidades correctas
        assertEquals(76, numberCards); // 19 por color * 4 colores = 76
        assertEquals(8, skipCards);    // 2 por color * 4 colores = 8
        assertEquals(8, reverseCards); // 2 por color * 4 colores = 8
        assertEquals(8, draw2Cards);   // 2 por color * 4 colores = 8
        assertEquals(4, wildCards);    // 4 cartas comodín
    }

    @Test
    public void testFullDeckContainsAllColors() {
        List<Card> deck = dealer.fullDeck();
        
        // Obtener todos los colores presentes (excluyendo WildCards sin color asignado)
        List<String> colors = deck.stream()
                .filter(card -> !(card instanceof WildCard))
                .map(Card::color)
                .distinct()
                .collect(Collectors.toList());
        
        assertEquals(4, colors.size());
        assertTrue(colors.contains("Red"));
        assertTrue(colors.contains("Blue"));
        assertTrue(colors.contains("Green"));
        assertTrue(colors.contains("Yellow"));
    }

    @Test
    public void testFullDeckNumberCardsDistribution() {
        List<Card> deck = dealer.fullDeck();
        
        // Filtrar solo cartas numéricas y agrupar por número
        Map<Integer, Long> numberCount = deck.stream()
                .filter(card -> card instanceof NumberCard)
                .map(card -> (NumberCard) card)
                .collect(Collectors.groupingBy(
                    card -> {
                        try {
                            // Usar reflexión para acceder al campo number
                            var field = NumberCard.class.getDeclaredField("number");
                            field.setAccessible(true);
                            return (Integer) field.get(card);
                        } catch (Exception e) {
                            return -1; // Valor por defecto si falla
                        }
                    },
                    Collectors.counting()
                ));
        
        // Verificar que hay 4 cartas de 0 (una por color)
        assertEquals(4, numberCount.get(0).intValue());
        
        // Verificar que hay 8 cartas de cada número del 1-9 (dos por color)
        for (int i = 1; i <= 9; i++) {
            assertEquals(8, numberCount.get(i).intValue());
        }
    }

    @Test
    public void testFullDeckIsShuffled() {
        List<Card> deck1 = dealer.fullDeck();
        List<Card> deck2 = dealer.fullDeck();
        
        // Es extremadamente improbable que dos mazos barajados sean idénticos
        // Verificamos que al menos las primeras 5 cartas sean diferentes
        boolean isDifferent = false;
        for (int i = 0; i < Math.min(5, deck1.size()); i++) {
            if (!deck1.get(i).equals(deck2.get(i))) {
                isDifferent = true;
                break;
            }
        }
        
        assertTrue(isDifferent, "Los mazos deberían estar barajados de forma diferente");
    }

    @Test
    public void testFullDeckConsistency() {
        // Generar múltiples mazos y verificar que todos tienen el mismo contenido
        List<Card> deck1 = dealer.fullDeck();
        List<Card> deck2 = dealer.fullDeck();
        
        // Aunque estén barajados diferente, deben tener el mismo tamaño
        assertEquals(deck1.size(), deck2.size());
        
        // Y el mismo tipo de cartas
        long numberCards1 = deck1.stream().filter(card -> card instanceof NumberCard).count();
        long numberCards2 = deck2.stream().filter(card -> card instanceof NumberCard).count();
        assertEquals(numberCards1, numberCards2);
        
        long wildCards1 = deck1.stream().filter(card -> card instanceof WildCard).count();
        long wildCards2 = deck2.stream().filter(card -> card instanceof WildCard).count();
        assertEquals(wildCards1, wildCards2);
    }
} 