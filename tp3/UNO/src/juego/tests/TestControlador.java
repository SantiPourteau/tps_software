//package juego.tests;// juego.tests.TestControlador.java
//import static org.junit.jupiter.api.Assertions.*;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import java.util.Arrays;
//import java.util.List;
//
//public class TestControlador {
//    private List<Jugador> jugadores;
//    private Ronda ronda;
//    private Controlador controladorDer;
//    private Controlador controladorIzq;
//
//    @BeforeEach
//    void setUp() {
//        jugadores = Arrays.asList(
//                new Jugador("P1"),
//                new Jugador("P2"),
//                new Jugador("P3")
//        );
//        ronda = new Ronda(jugadores);
//        controladorDer = new ControladorDerecha(ronda);
//        controladorIzq = new ControladorIzquierda(ronda);
//    }
//
//    @Test
//    void testControladorDerechaAvanza() {
//        assertEquals("P1", controladorDer.getJugadorActual().getNombre());
//        controladorDer.avanzar();
//        assertEquals("P2", controladorDer.getJugadorActual().getNombre());
//        controladorDer.avanzar();
//        assertEquals("P3", controladorDer.getJugadorActual().getNombre());
//        controladorDer.avanzar();
//        assertEquals("P1", controladorDer.getJugadorActual().getNombre());
//    }
//
//    @Test
//    void testControladorIzquierdaAvanza() {
//        assertEquals("P1", controladorIzq.getJugadorActual().getNombre());
//        controladorIzq.avanzar();
//        assertEquals("P3", controladorIzq.getJugadorActual().getNombre());
//        controladorIzq.avanzar();
//        assertEquals("P2", controladorIzq.getJugadorActual().getNombre());
//        controladorIzq.avanzar();
//        assertEquals("P1", controladorIzq.getJugadorActual().getNombre());
//    }
//}
