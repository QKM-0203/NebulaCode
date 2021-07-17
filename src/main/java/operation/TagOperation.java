package operation;

import Operator.Operator;
import entity.Graph;
import ogm.Tag;

import java.lang.reflect.Field;

public class TagOperation extends Graph {
    private Graph graph;

    public TagOperation(Graph graph){
        this.graph = graph;
    }


    public void create_Tag(Tag tag) throws NoSuchFieldException, IllegalAccessException {
        Class<? extends Tag> aClass = tag.getClass();
        Field name = aClass.getDeclaredField("name");
        //拼接nGql调用graph执行run
        name.setAccessible(true);
       // System.out.println(name.get(tag));
        graph.run(Operator.CREATE+Operator.TAG+Operator.IF_NOT_EXIST +name.get(tag)+Operator.LEFT_BRACKETS+Operator.RIGHT_BRACKETS);
    }

    public void drop_Tag(Tag tag) throws NoSuchFieldException, IllegalAccessException {
        Class<? extends Tag> aClass = tag.getClass();
        Field name = aClass.getDeclaredField("name");
        //拼接nGql调用graph执行run
        name.setAccessible(true);
        // System.out.println(name.get(tag));
        graph.run(Operator.CREATE+Operator.TAG+Operator.IF_NOT_EXIST +name.get(tag)+Operator.LEFT_BRACKETS+Operator.RIGHT_BRACKETS);
    }


    public void update_Tag(Tag tag) throws NoSuchFieldException, IllegalAccessException {
        Class<? extends Tag> aClass = tag.getClass();
        Field name = aClass.getDeclaredField("name");
        //拼接nGql调用graph执行run
        name.setAccessible(true);
        // System.out.println(name.get(tag));
        graph.run(Operator.CREATE+Operator.TAG+Operator.IF_NOT_EXIST +name.get(tag)+Operator.LEFT_BRACKETS+Operator.RIGHT_BRACKETS);
    }





}
