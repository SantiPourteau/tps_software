//package juego.tests;// juego.tests.TestJugador.java
//import static org.junit.jupiter.api.Assertions.*;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import java.util.Arrays;
//
//public class TestJugador {
//    private Jugador jugador;
//    private Carta topePozo;
//
//    @BeforeEach
//    void setUp() {
//        jugador = new Jugador("P1");
//        jugador.recibirCartas(Arrays.asList(
//                new CartaNumero("R", 1),
//                new CartaSkip("G"),
//                new CartaReverse("B")
//        ));
//        topePozo = new CartaNumero("R", 1);
//    }
//
//    @Test
//    void testJugadorJuegaCartaCompatible() {
//        Carta c = new CartaNumero("R", 1);
//        jugador.jugarCarta(c, topePozo);
//        assertFalse(jugador.tieneCarta(c), "Después de jugar una carta válida, no debe estar en la mano");
//    }
//
//    @Test
//    void testJugadorJuegaCartaNoCompatibleLanza() {
//        Carta c = new CartaDraw2("Y");
//        assertThrows(IllegalArgumentException.class,
//                () -> jugador.jugarCarta(c, topePozo),
//                "Jugar carta no compatible debe lanzar IllegalArgumentException");
//    }
//}
