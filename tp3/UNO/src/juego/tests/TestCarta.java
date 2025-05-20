//package juego.tests;
//
//import static org.junit.jupiter.api.Assertions.*;
//import org.junit.jupiter.api.Test;
//import java.util.Arrays;
//
//public class TestCarta {
//    @Test
//    void testNumeroCartaCompatiblePorColor() {
//        Carta c1 = new CartaNumero("R", 5);
//        Carta c2 = new CartaNumero("R", 3);
//        assertTrue(c1.esCompatibleCon(c2));
//    }
//
//    @Test
//    void testNumeroCartaCompatiblePorNumero() {
//        Carta c1 = new CartaNumero("R", 5);
//        Carta c2 = new CartaNumero("G", 5);
//        assertTrue(c1.esCompatibleCon(c2));
//    }
//
//    @Test
//    void testNumeroCartaIncompatible() {
//        Carta c1 = new CartaNumero("R", 5);
//        Carta c2 = new CartaNumero("G", 7);
//        assertFalse(c1.esCompatibleCon(c2));
//    }
//
//    @Test
//    void testSkipCompatiblePorColor() {
//        Carta c1 = new CartaSkip("B");
//        Carta c2 = new CartaNumero("B", 2);
//        assertTrue(c1.esCompatibleCon(c2));
//    }
//
//    @Test
//    void testSkipCompatibleConSkipPorTipo() {
//        Carta c1 = new CartaSkip("B");
//        Carta c2 = new CartaSkip("G");
//        assertTrue(c1.esCompatibleCon(c2));
//    }
//
//    @Test
//    void testSkipIncompatible() {
//        Carta c1 = new CartaSkip("B");
//        Carta c2 = new CartaDraw2("B");
//        assertFalse(c1.esCompatibleCon(c2));
//    }
//
//    @Test
//    void testWildCompatibleConCualquierCarta() {
//        Carta w = new CartaWild();
//        for (String color : Arrays.asList("R","G","B","Y")) {
//            Carta any = new CartaNumero(color, 1);
//            assertTrue(w.esCompatibleCon(any));
//        }
//    }
//
//    @Test
//    void testColoredWildCompatibilidad() {
//        Carta cw = new CartaColoredWild("G");
//        Carta any = new CartaNumero("G", 9);
//        assertTrue(cw.esCompatibleCon(any));
//    }
//}