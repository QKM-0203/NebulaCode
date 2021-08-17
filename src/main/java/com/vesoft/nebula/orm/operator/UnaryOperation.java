package com.vesoft.nebula.orm.operator;

public enum UnaryOperation implements Filter {
    IsNotNull("IS NOT NULL"),
    IsNull("IS NULL");

    String symbol;
    UnaryOperation(String symbol) {
        this.symbol = symbol;
    }

    public String getSymbol() {
        return symbol;
    }
}
