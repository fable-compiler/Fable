module Models
open System
    
type Comment = {
    id: DateTime option
    author: string
    text: string
}