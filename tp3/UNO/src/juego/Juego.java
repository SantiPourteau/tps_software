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
    public Controlador controlador;
    private List<Jugador> jugadores = new ArrayList<Jugador>();
    private String estado = "enCurso";


    public Juego(List<Carta> cartas, int numCartas, List<String> nombres) {
        //Inicializar mazo
        this.mazo = new LinkedList<>(cartas);
        // Carta inicial del pozo
        this.pozo = mazo.removeFirst();
        for (String nombre : nombres) {
            this.jugadores.addLast(new Jugador(nombre, new ArrayList<Carta>()));
        }
        this.controlador = new ControladorDerecha(this.jugadores).avanzar();
        // Reparto consecutivo
        for (Jugador j : jugadores) {
            for (int i = 0; i < numCartas; i++) {
                Carta carta = mazo.removeFirst();
                j.recibirCarta(carta);
            }
        }
    }

    public Juego jugarCarta(Carta carta) {
        if (estado.equals("finalizada")) {
            throw new IllegalStateException("El juego ya ha finalizado.");
        }
        Jugador actual = controlador.getJugadorActual();



        if (actual.jugarCarta(carta, this.pozo)){
            this.pozo = carta;
            if (actual.getManoSize() == 1){
                controlador = controlador.penalizarJugador(this, 2);
            }
            controlador = carta.actualizarControlador(this, controlador);
        }
        else {
            // no se puede jugar la carta devuelve exepcion
            controlador = controlador.penalizarJugador(this, 2).avanzar();
        }

        // Va aca un if getManoSize == 1 -> Robar carta?

        if (actual.manoVacia()) {
            estado = "finalizada";
        }
        return this;
    }


    public Juego jugarCartaCantandoUno(Carta carta) {
        if (estado.equals("finalizada")) {
            throw new IllegalStateException("El juego ya ha finalizado.");
        }
        Jugador actual = controlador.getJugadorActual();
        if (actual.getManoSize() != 2){
            controlador = controlador.penalizarJugador(this, 2);

        }
        if(actual.jugarCarta(carta, getCartaPozo())){
            pozo = carta;
            controlador = carta.actualizarControlador(this, controlador);
        }
        else {
            // levanta por jugar mal la carta
            controlador = controlador.penalizarJugador(this, 2).avanzar();

        }

        if (actual.manoVacia()) {
            estado = "finalizada";
        }

        return this;
    }

    public Juego jugarCartaCambiandoColor(String color) {
        if (estado.equals("finalizada")) {
            throw new IllegalStateException("El juego ya ha finalizado.");
        }
        Jugador actual = controlador.getJugadorActual();
        CartaWild carta = new CartaWild(color);
        if(actual.jugarCarta(carta, getCartaPozo())){
            pozo = carta;
            controlador = carta.actualizarControlador(this, controlador);
        }
        else {
            // levanta por jugar mal la carta
            controlador = controlador.penalizarJugador(this, 2);
        }
        return this;
    }

    public Juego levantarCartaMazo(){
        // La funcion que se llama cuando un jugador no puede tirar ninguna carta y tiene que levantar del mazo
        // El jugador puede decidir levantar una carta aunque pueda tirar.
        if (estado.equals("finalizada")) {
            throw new IllegalStateException("El juego ya ha finalizado.");
        }
        controlador = controlador.penalizarJugador(this,1);
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
    public Jugador getJugadorActual() {
        return controlador.getJugadorActual();
    }


    public List<Carta> getCartasJugador(String nombreJug) {
        for (Jugador j : jugadores) {
            if (j.nombre.equals(nombreJug)) {
                // Devolvemos una copia defensiva de la mano
                return new ArrayList<>(j.getMano());
            }
        }
        // Si no encontramos al jugador, devolvemos lista vacía
        return Collections.emptyList();
    }
}
