package juego;

import juego.controlador.Controlador;
import juego.controlador.ControladorDerecha;
import juego.jugador.Jugador;
import juego.carta.Carta;

import java.util.*;

/**
 * Clase principal que gestiona el estado del juego sin bucle interno.
 * Métodos retornan el mismo objeto para permitir encadenamiento.
 * Las cartas se reparten de forma consecutiva sin mezclar.
 */
public class Juego {
    private Controlador controlador;
    private Map<String, Jugador> jugadores;
    private Deque<Carta> mazo;
    private Deque<Carta> pilaDescarte;
    private Carta pozo;
    private int penalizacionCartas = 0;  // cartas a levantar por penalización
    private String estado = "enCurso";

    /**
     * Inicializa el juego:
     * @param cartas     lista inicial del mazo (orden predefinido)
     * @param numCartas  número de cartas a repartir a cada jugador (consecutivas)
     * @param nombres    nombres de los jugadores en orden de juego
     */
    public Juego(List<Carta> cartas, int numCartas, List<String> nombres) {
        this.controlador = new ControladorDerecha(nombres);
        this.jugadores = new LinkedHashMap<>();
        for (String nombre : nombres) {
            this.jugadores.put(nombre, new Jugador(new ArrayList<>()));
        }
        int idx = 0;
        // Reparto consecutivo
        for (String nombre : nombres) {
            Jugador j = jugadores.get(nombre);
            for (int i = 0; i < numCartas; i++) {
                j.recibirCarta(cartas.get(idx));
                idx++;
            }
        }
        // Carta inicial del pozo
        this.pozo = cartas.get(idx);
        idx++;
        this.pilaDescarte = new ArrayDeque<>();
        pilaDescarte.push(pozo);
        // Resto al mazo
        List<Carta> resto = cartas.subList(idx, cartas.size());
        this.mazo = new ArrayDeque<>(resto);
    }

    /**
     * Juega una carta del jugador actual.
     * Si tenía 2 cartas antes, recibe penalización.
     * Si se queda sin cartas, termina el juego.
     */
    public Juego jugarCarta(Carta carta) {
        String actual = controlador.getJugadorActual();
        Jugador j = jugadores.get(actual);
        int manoAntes = j.getManoSize();
        j.jugarCarta(carta);
        pilaDescarte.push(carta);
        pozo = carta;
        // Acumula penalización según tipo de carta
        penalizacionCartas += carta.penalizar();
        // Si no cantó Uno (tenía 2 cartas), se penaliza
        if (manoAntes == 2) {
            robarCartas(penalizacionCartas);
            penalizacionCartas = 0;
        }
        // Verifica victoria
        if (j.manoVacia()) {
            estado = "finalizada";
        }
        carta.applyEffect(controlador, mazo, jugadores);
        return this;
    }

    /**
     * Juega una carta cantando Uno.
     * Si no tenía 2 cartas antes, recibe penalización de 2 cartas.
     */
    public Juego jugarCartaCantandoUno(Carta carta) {
        String actual = controlador.getJugadorActual();
        Jugador j = jugadores.get(actual);
        int manoAntes = j.getManoSize();
        j.jugarCarta(carta);
        // Penaliza si no cantó en el momento adecuado
        if (manoAntes != 2) {
            robarCartas(2);
        }
        j.cantarUno();
        pilaDescarte.push(carta);
        pozo = carta;
        penalizacionCartas += carta.penalizar();
        carta.applyEffect(controlador, mazo, jugadores);
        return this;
    }

    /**
     * Cambia el color de la carta Wild que esté en el pozo.
     */
    public Juego jugarCartaCambiandoColor(String color) {
        pozo.cambiarColor(color);
        return this;
    }

    /**
     * El jugador actual roba cartas por penalización o efectos.
     * No avanza el turno del controlador.
     */
    public Juego robarCartas(int cantidad) {
        String actual = controlador.getJugadorActual();
        Jugador j = jugadores.get(actual);
        for (int i = 0; i < cantidad; i++) {
            j.recibirCarta(mazo.pop());
        }
        return this;
    }

    public int getCantidadCartas(String nombre) {
        return jugadores.get(nombre).getManoSize();
    }

    public Carta getCartaPozo() {
        return pozo;
    }

    public String getJugadorActual() {
        return controlador.getJugadorActual();
    }

    public String getEstado() {
        return estado;
    }
}
