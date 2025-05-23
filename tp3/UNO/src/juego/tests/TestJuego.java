package juego.tests;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import juego.Juego;
import juego.carta.Carta;
import juego.carta.CartaNumero;
import juego.carta.CartaReverse;
import juego.carta.CartaDraw2;
import juego.carta.CartaSkip;
import juego.carta.CartaWild;

public class TestJuego {

    private Juego juego;
    private Carta cPozo;
    private List<Carta> mazo;
    private List<String> jugadores;

    @BeforeEach
    public void setUp() {
        // Construimos un mazo determinista: primera carta para pozo, luego cartas para repartir
        mazo = new LinkedList<>();
        cPozo = new CartaNumero("rojo", 0);
        mazo.add(cPozo);
        // Reparto inicial: 2 cartas por jugador
        mazo.add(new CartaNumero("rojo", 1));
        mazo.add(new CartaNumero("azul", 2));
        mazo.add(new CartaNumero("verde", 3));
        mazo.add(new CartaNumero("amarillo", 4));
        jugadores = Arrays.asList("Alice", "Bob");
        juego = new Juego(mazo, 2, jugadores);
    }

    public static Juego juegoGanado() {
        // Configuración con 1 carta inicial: al jugarla, se finaliza el juego
        List<Carta> m2 = new LinkedList<>();
        Carta initial = new CartaNumero("azul", 9);
        m2.add(initial);
        m2.add(new CartaNumero("azul", 1));
        m2.add(new CartaNumero("rojo", 1));
        List<String> dosJug = Arrays.asList("P1", "P2");
        Juego g2 = new Juego(m2, 1, dosJug);
        // P1 juega su única carta compatible
        g2.jugarCarta(new CartaNumero("azul", 1));
        return g2;
    }

    @Test
    public void testCantidadCartasCorrectas() {
        assertEquals(2, juego.getCantidadCartas("Alice"));
        assertEquals(2, juego.getCantidadCartas("Bob"));
    }

    @Test
    public void testPrimerCartaPozoCorrecta() {
        Carta top = juego.getCartaPozo();
        assertInstanceOf(CartaNumero.class, top);
        assertEquals("rojo", ((CartaNumero) top).color);
        assertEquals(0, ((CartaNumero) top).numero);
    }

    @Test
    public void testRobarCartasIniciales() {
        // Bob tiene 2 cartas, roba 1 más
        juego.mazo.add(new CartaNumero("amarillo", 9));
        juego.levantarCartaMazo();
        assertEquals(3, juego.getCantidadCartas("Alice"));
    }

    @Test
    public void testInvertirOrden() {
        // Usamos 3 jugadores para comprobar inversión de sentido
        List<String> tresJug = Arrays.asList("A", "B", "C");
        List<Carta> mazo3 = new LinkedList<>();
        mazo3.add(new CartaNumero("rojo", 0));

        // Reparto 1 carta cada uno
        mazo3.add(new CartaReverse("rojo"));
        mazo3.add(new CartaNumero("amarillo", 2));
        mazo3.add(new CartaNumero("azul", 3));
        Juego juego3 = new Juego(mazo3, 1, tresJug);

        juego3.jugarCarta(new CartaReverse("rojo"));
        assertEquals("C", juego3.getJugadorActual().nombre);
    }

    @Test
    public void testGanarJuego() {
        Juego g2 = juegoGanado();
        assertEquals("finalizada", g2.getEstado());
    }



    @Test
    public void testPenalizacionPorNoCantarUno() {
        // Jugador con 2 cartas, juega una sin cantar uno y debe robar 2 cartas de penalización
        List<Carta> m3 = new LinkedList<>();
        Carta p = new CartaNumero("amarillo", 5);
        m3.add(p);
        m3.add(new CartaNumero("amarillo", 6));
        m3.add(new CartaNumero("azul", 7));
        m3.add(new CartaNumero("rojo", 8));
        m3.add(new CartaNumero("rojo", 9));
        List<String> one = Collections.singletonList("P");
        Juego jp = new Juego(m3, 2, one);
        // Juega la carta dejando 1 en mano
        jp.jugarCarta(new CartaNumero("amarillo", 6));
        // Se penaliza con 2 cartas
        assertEquals(3, jp.getCantidadCartas("P"));
    }

    @Test
    public void testNoSePuedeJugarDespuesDeFinalizar() {
        Juego juego = juegoGanado();
        assertThrows(IllegalStateException.class,
                () -> juego.jugarCarta(new CartaNumero("rojo", 1)));
    }

    @Test
    public void testRobarCartasDespuesDeFinalizar() {
        // Tras finalizar, robar cartas no está permitido
        Juego juego = juegoGanado();
        assertThrows(IllegalStateException.class,
                () -> juego.levantarCartaMazo());
    }

    @Test
    public void testJugarCartaQueNoTienes() {
        // Intentar jugar carta que no está en la mano lanza excepción
        assertThrows(IllegalArgumentException.class,
                () -> juego.jugarCarta(new CartaNumero("verde", 99)));
    }

    // Tests de validez de jugadas combinando tipos de carta

    @Test
    public void testValidNumberWithNumberDifferentColour() {
        CartaNumero c1 = new CartaNumero("rojo", 5);
        CartaNumero c2 = new CartaNumero("azul", 5);
        assertTrue(c1.esCompatible(c2));
    }

    @Test
    public void testValidNumberWithNumberSameColourDifferentNumber() {
        CartaNumero c1 = new CartaNumero("rojo", 5);
        CartaNumero c2 = new CartaNumero("rojo", 7);
        assertTrue(c1.esCompatible(c2));
    }

    @Test
    public void testInvalidNumberWithNumberDifferentColourAndDifferentNumber() {
        CartaNumero c1 = new CartaNumero("rojo", 5);
        CartaNumero c2 = new CartaNumero("azul", 7);
        assertFalse(c1.esCompatible(c2));
    }

    @Test
    public void testValidDrawTwoWithDrawTwoDifferentColour() {
        CartaDraw2 d1 = new CartaDraw2("rojo");
        CartaDraw2 d2 = new CartaDraw2("verde");
        assertTrue(d1.esCompatible(d2));
    }

    @Test
    public void testValidDrawTwoWithNumberSameColour() {
        CartaDraw2 d = new CartaDraw2("rojo");
        CartaNumero n = new CartaNumero("rojo", 9);
        assertTrue(d.esCompatible(n));
    }

    @Test
    public void testInvalidDrawTwoWithNumberDifferentColour() {
        CartaDraw2 d = new CartaDraw2("rojo");
        CartaNumero n = new CartaNumero("azul", 9);
        assertFalse(d.esCompatible(n));
    }

    @Test
    public void testValidReverseWithReverseDifferentColour() {
        CartaReverse r1 = new CartaReverse("rojo");
        CartaReverse r2 = new CartaReverse("azul");
        assertTrue(r1.esCompatible(r2));
    }

    @Test
    public void testValidReverseWithNumberSameColour() {
        CartaReverse r = new CartaReverse("rojo");
        CartaNumero n = new CartaNumero("rojo", 3);
        assertTrue(r.esCompatible(n));
    }

    @Test
    public void testInvalidReverseWithNumberDifferentColour() {
        CartaReverse r = new CartaReverse("rojo");
        CartaNumero n = new CartaNumero("azul", 3);
        assertFalse(r.esCompatible(n));
    }

    @Test
    public void testValidSkipWithSkipDifferentColour() {
        CartaSkip s1 = new CartaSkip("amarillo");
        CartaSkip s2 = new CartaSkip("verde");
        assertTrue(s1.esCompatible(s2));
    }

    @Test
    public void testValidSkipWithNumberSameColour() {
        CartaSkip s = new CartaSkip("amarillo");
        CartaNumero n = new CartaNumero("amarillo", 4);
        assertTrue(s.esCompatible(n));
    }

    @Test
    public void testInvalidSkipWithNumberDifferentColour() {
        CartaSkip s = new CartaSkip("amarillo");
        CartaNumero n = new CartaNumero("azul", 4);
        assertFalse(s.esCompatible(n));
    }

    @Test
    public void testValidWildWithNumber() {
        CartaWild w = new CartaWild();
        CartaNumero n = new CartaNumero("rojo", 2);
        assertTrue(w.esCompatible(n));
    }

    @Test
    public void testValidWildWithDrawTwo() {
        CartaWild w = new CartaWild();
        CartaDraw2 d = new CartaDraw2("verde");
        assertTrue(w.esCompatible(d));
    }

    @Test
    public void testValidWildWithReverse() {
        CartaWild w = new CartaWild();
        CartaReverse r = new CartaReverse("azul");
        assertTrue(w.esCompatible(r));
    }

    @Test
    public void testValidWildWithSkip() {
        CartaWild w = new CartaWild();
        CartaSkip s = new CartaSkip("amarillo");
        assertTrue(w.esCompatible(s));
    }

    @Test
    public void testValidWildWithWild() {
        CartaWild w1 = new CartaWild();
        CartaWild w2 = new CartaWild("rojo");
        assertTrue(w1.esCompatible(w2));
    }
}