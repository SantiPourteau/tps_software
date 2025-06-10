package com.example.tp4.exceptions;

/**
 * Excepción para cuando no se encuentra una partida.
 * Se usa cuando se busca una partida con un ID que no existe.
 * Extiende RuntimeException porque es un error de bajo nivel.
 */
public class MatchNotFoundException extends RuntimeException {
    
    public MatchNotFoundException(String matchId) {
        super("Partida no encontrada: " + matchId);
    }
    
    public MatchNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
} 