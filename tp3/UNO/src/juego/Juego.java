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
    private List<Carta> mazo;
    private Carta pozo;
    private Controlador controlador;
    private List<Jugador> jugadores = new ArrayList<Jugador>();
    private String estado = "enCurso";
    /**
     * Inicializa el juego:
     * @param cartas     lista inicial del mazo (orden predefinido)
     * @param numCartas  número de cartas a repartir a cada jugador (consecutivas)
     * @param nombres    nombres de los jugadores en orden de juego
     */
    public Juego(List<Carta> cartas, int numCartas, List<String> nombres) {
        for (String nombre : nombres) {
            this.jugadores.add(new Jugador(nombre, new ArrayList<Carta>()));
        }
        this.controlador = new ControladorDerecha(this.jugadores);
        int idx = 0;
        // Reparto consecutivo
        for (Jugador j : jugadores) {
            for (int i = 0; i < numCartas; i++) {
                j.recibirCarta(mazo.robarCarta());
            }
        }
        // Carta inicial del pozo
        this.pozo = cartas.remove(idx);
        idx++;
        this.mazo = cartas;
    }

    /**
     * Juega una carta del jugador actual.
     * Si tenía 2 cartas antes, recibe penalización.
     * Si se queda sin cartas, termina el juego.
     */
    public Juego jugarCarta(Carta carta) {
        Jugador actual = controlador.getJugadorActual();
        int manoAntes = actual.getManoSize();
        actual.jugarCarta(carta);
        pozo = carta;
        if (actual.manoVacia()) {
            estado = "finalizada";
        }
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
