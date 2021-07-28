package entity;

public class Part {
    private Vertex startVertex;
    private Vertex endVertex;
    private Relationship relationship;

    public Part(Vertex startVertex, Relationship relationship, Vertex endVertex) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.relationship = relationship;
    }

    public Vertex getStartVertex() {
        return startVertex;
    }

    public Vertex getEndVertex() {
        return endVertex;
    }

    public Relationship getRelationship() {
        return relationship;
    }
}
