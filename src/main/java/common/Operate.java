package common;

public  interface Operate<T>  {
    void create(T Entity) throws NoSuchFieldException, IllegalAccessException;
    void drop(T Entity) throws NoSuchFieldException, IllegalAccessException;
    void update(T Entity);
}
