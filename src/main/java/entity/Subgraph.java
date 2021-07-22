package entity;

import Operator.EdgeDirection;

import java.util.List;

public class Subgraph {
    private List<Object> srcVidList;

    //must be a non negative integer.The default value is 1.
    private int stepCount = 1;

    private EdgeDirection edgeDirectionDirection;

    public Subgraph(List<Object> srcVidList, int stepCount, EdgeDirection edgeDirectionDirection) {
        this.srcVidList = srcVidList;
        this.stepCount = stepCount;
        this.edgeDirectionDirection = edgeDirectionDirection;
    }

    public void setSrcVidList(List<Object> srcVidList) {
        this.srcVidList = srcVidList;
    }

    public void setStepCount(int stepCount) {
        this.stepCount = stepCount;
    }

    public void setEdgeTypeDirection(EdgeDirection edgeDirectionDirection) {
        this.edgeDirectionDirection = edgeDirectionDirection;
    }
}
