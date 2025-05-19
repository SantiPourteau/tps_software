package juego;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;

public class TestJuego {

    private Juego juego;
    private List<String> jugadores;
    private List<String> mazoDefault;
    private int cartasPorJugadorDefault;

    // Helper para crear mazos
    private List<String> deck(String... cartas) {
        return new ArrayList<>(Arrays.asList(cartas));
    }

    @BeforeEach
    public void setup() {
        // Jugadores fijos para todos los tests
        jugadores = List.of("P1", "P2", "P3");
        // Cada jugador recibe 3 cartas
        cartasPorJugadorDefault = 3;
        // Mazo por defecto de 20 cartas incluyendo especiales
        mazoDefault = deck(
                // Numéricas
                "R0","R1","R2","B0","B1","B2","G0","G1","Y0","Y1",
                // Repeticiones para completar
                "R3","B3","G2","Y2","Y3","G3",
                // Especiales
                "RRev","RS","RDraw2","W"
        );
        // Inicializo el juego una sola vez en setup
        juego = new Juego(new ArrayList<>(mazoDefault), new ArrayList<>(jugadores), cartasPorJugadorDefault);
    }

    @Test
    public void testInicializacionYDistribucionBloque() {
        // La primera carta del mazoDefault es la última carta jugada
        assertEquals(mazoDefault.get(0), juego.getUltimaCarta());
        // Se reparten bloques de 3 cartas
        List<List<String>> manos = juego.getManos();
        assertEquals(3, manos.size());
        assertEquals(mazoDefault.subList(1, 4), manos.get(0));
        assertEquals(mazoDefault.subList(4, 7), manos.get(1));
        assertEquals(mazoDefault.subList(7, 10), manos.get(2));
        assertEquals(0, juego.getIndiceJugadorActual());
    }

    @Test
    public void testGetJugadorInicial() {
        assertEquals("P1", juego.getJugador());
    }

    @Test
    public void testJugarCartaValidaAvanzaTurnoYActualizaUltimaCarta() {
        // Jugador P1 tiene en mano la primera carta de su mano
        String cartaValida = juego.getManos().get(0).get(0);
        // Debe coincidir en color con ultimaCarta
        juego.jugarCarta(cartaValida);

        assertEquals(cartaValida, juego.getUltimaCarta());
        assertEquals("P2", juego.getJugador());
    }

    @Test
    public void testJugarCartaInvalidaLanzaException() {
        // Uso carta que no coincide ni está en su mano ni color/valor
        assertThrows(IllegalArgumentException.class, () -> juego.jugarCarta("X9"));
    }

    @Test
    public void testJugarCartaWildConColorAvanzaTurno() {
        // Añado Wild al juego manualmente para el test
        String wild = "W";
        juego = new Juego(deck("R0",wild,"B1","G2","Y3","R1","B2","G3","Y4","R2",
                "B3","G0","Y0","R3","B0","G1","Y1","R4","B4","Y5"),
                jugadores, cartasPorJugadorDefault);
        // P1 juega Wild cantando color G
        juego.jugarCartaCantandoColor(wild, "G");

        assertEquals(wild, juego.getUltimaCarta());
        assertEquals("P2", juego.getJugador());
    }

    @Test
    public void testPenalizacionPorNoCantarUno() {
        // Después de jugar dos cartas, mano baja a 1 => penalización
        // Jugadores reciben 3 cartas, juego tiene configuración por defecto
        List<String> manoInicial = juego.getManos().get(0);
        juego.jugarCarta(manoInicial.get(0));
        juego.jugarCarta(manoInicial.get(1));
        // Ahora P1 tendría 1 carta pero no cantó Uno
        assertEquals(3, juego.getManos().get(0).size());
        assertEquals("P2", juego.getJugador());
    }

    @Test
    public void testJugarCartaCantandoUnoPrevienePenalizacion() {
        // P1 juega dos cartas cantando Uno en la segunda jugada
        List<String> mano = new ArrayList<>(juego.getManos().get(0));
        juego.jugarCarta(mano.get(0)); // mano=2
        juego.jugarCartaCantandoUno(mano.get(1)); // canta Uno al jugar la penúltima

        assertEquals(1, juego.getManos().get(0).size());
        assertEquals("P2", juego.getJugador());
    }

    @Test
    public void testTurnCycleMultiplePlays() {
        // Con 3 jugadores, debería ciclar P1->P2->P3->P1
        String carta = juego.getManos().get(0).get(0);
        juego.jugarCarta(carta); assertEquals("P2", juego.getJugador());
        carta = juego.getManos().get(1).get(0);
        juego.jugarCarta(carta); assertEquals("P3", juego.getJugador());
        carta = juego.getManos().get(2).get(0);
        juego.jugarCarta(carta); assertEquals("P1", juego.getJugador());
    }

    @Test
    public void testDistribucionBloqueNoRoundRobin() {
        // Verifica que se reparten bloques y no en ronda usando mazoDefault
        // mano P1 = mazoDefault[1..3], mano P2 = mazoDefault[4..6], mano P3 = mazoDefault[7..9]
        List<List<String>> manos = juego.getManos();
        assertEquals(mazoDefault.subList(1, 4), manos.get(0));
        assertEquals(mazoDefault.subList(4, 7), manos.get(1));
        assertEquals(mazoDefault.subList(7, 10), manos.get(2));
    }
}
