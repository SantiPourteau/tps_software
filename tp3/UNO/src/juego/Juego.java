import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Clase que representa el mazo de cartas de Uno.
 * Encapsula operaciones de barajar y robar cartas.
 */
public class Mazo {
    private List<Carta> cartas;

    /**
     * Crea un mazo a partir de la lista de cartas dada.
     * @param cartasIniciales Lista de cartas para poblar el mazo.
     */
    public Mazo(List<Carta> cartasIniciales) {
        this.cartas = new ArrayList<>(cartasIniciales);
    }

    /**
     * Baraja el mazo de forma aleatoria.
     */
    public void barajar() {
        Collections.shuffle(cartas);
    }

    /**
     * Roba la carta superior del mazo.
     * @return Carta robada.
     * @throws IllegalStateException si el mazo está vacío.
     */
    public Carta robarCarta() {
        if (cartas.isEmpty()) throw new IllegalStateException("El mazo está vacío");
        return cartas.remove(0);
    }

    /**
     * Roba varias cartas del mazo.
     * @param cantidad Número de cartas a robar.
     * @return Lista con las cartas robadas.
     * @throws IllegalStateException si no hay suficientes cartas.
     */
    public List<Carta> robarCartas(int cantidad) {
        if (cartas.size() < cantidad) throw new IllegalStateException("No hay suficientes cartas en el mazo");
        List<Carta> robadas = new ArrayList<>();
        for (int i = 0; i < cantidad; i++) {
            robadas.add(robarCarta());
        }
        return robadas;
    }

    /**
     * Revierte las cartas de descarte al mazo (excepto la carta superior).
     * @param descarte Lista de cartas descartadas (pila de descarte completa).
     */
    public void recargarDesdeDescarte(List<Carta> descarte) {
        if (descarte.isEmpty()) return;
        Carta tope = descarte.remove(descarte.size() - 1);
        cartas.addAll(descarte);
        descarte.clear();
        descarte.add(tope);
        barajar();
    }

    /**
     * Verifica si el mazo está vacío.
     */
    public boolean estaVacio() {
        return cartas.isEmpty();
    }
}


/**
 * Clase para gestionar la lógica de una partida de Uno.
 */
public class Juego {
    private Mazo mazo;
    private List<List<Carta>> manos;      // Manos de cada jugador
    private int cartasPorJugador;
    private List<String> jugadores;
    private String estado;                // "enCurso" o "finalizada"
    private int indiceJugadorActual;      // Índice del jugador cuyo turno es
    private int direccion;                // +1: sentido horario, -1: sentido antihorario
    private List<Carta> pilaDescarte;

    public Juego(Mazo mazo, int cartasPorJugador, List<String> jugadores) {
        this.mazo = mazo;
        this.cartasPorJugador = cartasPorJugador;
        this.jugadores = new ArrayList<>(jugadores);
        this.estado = "enCurso";
        this.indiceJugadorActual = 0;
        this.direccion = 1;
        this.manos = new ArrayList<>();
        this.pilaDescarte = new ArrayList<>();

        mazo.barajar();
        inicializarManos();
        repartirCartas();
        iniciarDescarte();
    }

    public void tirarCarta(Carta carta) {
        // TODO: validar carta vs cima de pilaDescarte
        manos.get(indiceJugadorActual).remove(carta);
        pilaDescarte.add(carta);
        // Aplicar efecto si es especial
    }

    public void tirarYCantarUno() {
        // TODO: verificar condición antes de llamar a tirarCarta
    }

    public void pasarTurno() {
        indiceJugadorActual = (indiceJugadorActual + direccion + jugadores.size()) % jugadores.size();
    }

    private void aplicarEfectoEspecial(Carta carta) {
        // Ejemplo de uso de mazo.robarCartas() y mazo.recargarDesdeDescarte()
    }

    private void inicializarManos() {
        for (int i = 0; i < jugadores.size(); i++) masón.add(new ArrayList<>());
    }

    private void repartirCartas() {
        for (int i = 0; i < cartasPorJugador; i++) {
            for (int j = 0; j < jugadores.size(); j++) {
                manos.get(j).add(mazo.robarCarta());
            }
        }
    }

    private void iniciarDescarte() {
        pilaDescarte.add(mazo.robarCarta());
    }
}
