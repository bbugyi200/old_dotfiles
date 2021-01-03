from typing import Any, Dict, Optional

from fastapi import FastAPI


app = FastAPI()


@app.get("/")
def read_root() -> Dict[str, Any]:
    return {"Hello": "World"}


@app.get("/items/{item_id}")
def read_item(item_id: int, q: Optional[str] = None) -> Dict[str, Any]:
    return {"item_id": item_id, "q": q}
