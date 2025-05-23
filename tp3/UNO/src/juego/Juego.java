package juego;

import juego.carta.CartaWild;
import juego.controlador.Controlador;
import juego.controlador.ControladorDerecha;
import juego.jugador.Jugador;
import juego.carta.Carta;

import java.util.*;

// Hay que ver como delegar el cantar uno a Carta

/**
 * Clase principal que gestiona el estado del juego sin bucle interno.
 * Métodos retornan el mismo objeto para permitir encadenamiento.
 * Las cartas se reparten de forma consecutiva sin mezclar.
 */
public class Juego {
    public Deque<Carta> mazo;
    private Carta pozo;
    private Controlador controlador;
    private List<Jugador> jugadores = new ArrayList<Jugador>();
    private String estado = "enCurso";


    public Juego(List<Carta> cartas, int numCartas, List<String> nombres) {
        //Inicializar mazo
        this.mazo = new LinkedList<>(cartas);
        // Carta inicial del pozo
        this.pozo = cartas.removeFirst();
        for (String nombre : nombres) {
            this.jugadores.add(new Jugador(nombre, new ArrayList<Carta>()));
        }
        this.controlador = new ControladorDerecha(this.jugadores);
        // Reparto consecutivo
        for (Jugador j : jugadores) {
            for (int i = 0; i < numCartas; i++) {
                Carta carta = mazo.removeFirst();
                j.recibirCarta(carta);
            }
        }
    }

    public Juego jugarCarta(Carta carta) {
        Jugador actual = controlador.getJugadorActual();
        int manoAntes = actual.getManoSize();
        if (actual.jugarCarta(carta, getCartaPozo())){
            pozo = carta;
            carta.actualizarControlador(this, controlador);
        }
        // Va aca un if getManoSize == 1 -> Robar carta?
        if (actual.manoVacia()) {
            estado = "finalizada";
        }
        return this;
    }


    public Juego jugarCartaCantandoUno(Carta carta) {
        Jugador actual = controlador.getJugadorActual();
        int manoAntes = actual.getManoSize();
        if(actual.jugarCarta(carta, getCartaPozo())){
            pozo = carta;
            carta.actualizarControlador(this, controlador);
        }
        return this;
    }

    public Juego jugarCartaCambiandoColor(String color) {
        Jugador actual = controlador.getJugadorActual();
        int manoAntes = actual.getManoSize();
        CartaWild carta = new CartaWild(color);
        if(actual.jugarCarta(carta, getCartaPozo())){
            pozo = carta;
            carta.actualizarControlador(this, controlador);
        }
        return this;
    }

    public Juego levantarCartaMazo(){
        // La funcion que se llama cuando un jugador no puede tirar ninguna carta y tiene que levantar del mazo
        Jugador actual = controlador.getJugadorActual();
        controlador.penalizarJugador(this,1);
        controlador.avanzar();
        return this;
    }

    public int getCantidadCartas(String nombreJug) {
        return jugadores.stream()
                .filter(j -> j.nombre.equals(nombreJug))
                .findFirst()
                .map(Jugador::getManoSize)
                .orElse(0);
    }

    public Carta getCartaPozo() {
        return pozo;
    }

    public String getEstado() {
        return estado;
    }
}
