module Models
open System

// Model shared between server and client
type Comment = {
    id: DateTime option
    author: string
    text: string
}